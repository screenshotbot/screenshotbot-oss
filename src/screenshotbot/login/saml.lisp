;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/saml
  (:use #:cl)
  (:import-from #:core/installation/auth-provider
                #:default-oidc-provider
                #:auth-provider-signin-form
                #:auth-provider)
  (:import-from #:auth/login/roles-auth-provider
                #:roles-auth-provider)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:util/request
                #:*engine*
                #:http-request)
  (:import-from #:bknr.indices
                #:indexed-class
                #:base-indexed-object)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-set-index
                #:fset-unique-index)
  (:import-from #:bknr.datastore
                #:with-transaction
                #:store-object
                #:persistent-class)
  (:import-from #:screenshotbot/model/company
                #:get-singleton-company
                #:company)
  (:import-from #:screenshotbot/installation
                #:site-admins-can-use-sso-p
                #:multi-org-feature)
  (:import-from #:screenshotbot/model/user
                #:adminp
                #:single-company-user)
  (:import-from #:core/installation/installation
                #:*installation*
                #:installation-domain)
  (:import-from #:util/store/object-id
                #:find-by-oid
                #:object-with-oid)
  (:import-from #:screenshotbot/login/common
                #:oauth-signin-link)
  (:import-from #:oidc/oidc
                #:end-session-endpoint
                #:logout-link)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:util/cron
                #:def-cron)
  (:export
   #:saml-auth-provider
   #:entity-id
   #:callback-url
   #:test-signin-link
   #:last-saml-test-result
   #:saml-test-result-email
   #:saml-test-result-error
   #:saml-test-result-ts
   #:saml-test-result-tested-by
   #:defaultp
   #:activation-blocker
   #:activate-saml-provider
   #:deactivate-saml-provider
   #:edit-saml-provider
   #:delete-saml-provider
   #:saml-idp-metadata-url
   #:saml-name))
(in-package :screenshotbot/login/saml)

(named-readtables:in-readtable markup:syntax)

(defvar *lock* (bt:make-lock))

(defindex +id-index+
  'fset-unique-index
  :slot-name 'id)

(defindex +company-index+
  'fset-set-index
  :slot-name '%company)

(defindex +cert-company-index+
  'fset-unique-index
  :slot-name '%company)

(defclass saml-cert (store-object)
  ((private-key :initarg :private-key
                :reader saml-private-key)
   (cert :initarg :public-cert
         :reader saml-public-cert)
   (%company :initarg :company
             :reader saml-cert-company
             :index +cert-company-index+
             :index-reader saml-cert-for-company))
  (:metaclass persistent-class)
  (:documentation "The singleton private-key/cert used for SAML"))

(defclass saml-installation-mixin ()
  ())

(defmethod default-oidc-provider :around ((self saml-installation-mixin))
  (or
   (default-saml-auth-provider)
   (call-next-method)))

(defun make-cert-pair ()
  (tmpdir:with-tmpdir (dir)
    (uiop:run-program
     (list
      "openssl"
      "req"
      "-newkey"
      "rsa:2048"
      "-nodes" "-keyout" (namestring (path:catfile dir "sp.key"))
      "-x509" "-days" "3650" 
      "-out" (namestring (path:catfile dir "sp.crt"))
      "-subj" "/CN=screenshotbot"))
    (uiop:run-program
     (list
      "openssl"
      "pkcs8"
      "-topk8"
      "-inform"
      "pem"
      "-nocrypt"
      "-in" (namestring (path:catfile dir "sp.key"))
      "-outform" "pem"
      "-out" (namestring (path:catfile dir "sp.pem"))))
    (values
     (uiop:read-file-string
      (path:catfile dir "sp.crt"))
     (uiop:read-file-string
      (path:catfile dir "sp.key")))))

(defun make-new-saml-cert (company)
  (multiple-value-bind (cert key)
      (make-cert-pair)
    (make-instance 'saml-cert
                   :company company
                   :public-cert cert
                   :private-key key)))

(defun find-or-create-saml-cert (company)
  (bt:with-lock-held (*lock*)
    (or
     (saml-cert-for-company company)
     (make-new-saml-cert company))))

(define-condition relay-state-expired (error)
  ())

(defun valid-relay-state-p (id)
  (and (str:non-empty-string-p id)
       (cl-ppcre:scan-to-strings
        "^\\d*_\\d*$" id)))

(defun relay-state-for-id (id)
  (cond
    ((valid-relay-state-p id)
     (let ((result (%relay-state-for-id id)))
       (cond
         (result
          ;; clear from index
          (setf (relay-state-id result)
                nil)
          result)
         (t
          (error 'relay-state-expired)))))
    (t
     (log:info "Invalid relay state: '~a'" id) ;; could be blank too
     nil)))

(defclass relay-state (base-indexed-object)
  ((id :initarg :id
       :index +id-index+
       :index-reader %relay-state-for-id
       :index-mapvalues map-relay-states
       :accessor relay-state-id
       :initform (format nil "~a_~a" (get-universal-time) (secure-random:number 10000000000000000)))
   (saml-auth-provider :initarg :saml-auth-provider
                       :reader saml-auth-provider)
   (settings :initarg :settings
             :reader relay-state-settings)
   (original-session-token :initarg :original-session-token
                           :reader original-session-token)
   (redirect :initarg :redirect
             :initform nil
             :reader redirect)
   (tested-by :initarg :tested-by
              :initform nil
              :reader relay-state-tested-by
              :documentation "If non-NIL, this is a test login started by this user. A test
login only records a SAML-TEST-RESULT, it does not log anyone in.")
   (created-at :initform (get-universal-time)
               :reader created-at))
  (:metaclass indexed-class))

(defindex +test-result-provider-index+
  'fset-set-index
  :slot-name '%provider)

(defclass saml-test-result (store-object)
  ((%provider :initarg :provider
              :index +test-result-provider-index+
              :index-reader saml-test-results-for-provider)
   (tested-by :initarg :tested-by
              :reader saml-test-result-tested-by)
   (email :initarg :email
          :initform nil
          :reader saml-test-result-email
          :documentation "The NameID from a successful test")
   (error-message :initarg :error-message
                  :initform nil
                  :reader saml-test-result-error
                  :documentation "Why the response failed to validate, NIL on success")
   (ts :initarg :ts
       :initform (get-universal-time)
       :reader saml-test-result-ts))
  (:metaclass persistent-class)
  (:documentation "The outcome of a test login against a SAML provider, used to make
sure a provider works before it's activated.

These are deleted along with the provider, see DELETE-SAML-PROVIDER.
TODO: activating a provider should require a successful test by the
activating admin."))

(defun last-saml-test-result (provider)
  ;; Not by TS: that's in seconds, so two tests in the same second
  ;; would be ambiguous. Object IDs increase with creation order.
  (let ((results (fset:convert 'list (saml-test-results-for-provider provider))))
    (first (sort results #'> :key #'bknr.datastore:store-object-id))))

(defindex +default-provider-index+
  'fset-unique-index
  :slot-name '%defaultp)

(defclass saml-auth-provider (auth-provider
                              object-with-oid)
  ((idp-metadata-url :initarg :idp-metadata-url
                     :reader idp-metadata-url
                     :writer (setf %idp-metadata-url))
   (metadata-xml :initform nil
                 :initarg :metadata-xml
                 :accessor %metadata-xml)
   (%defaultp :initform nil
              :initarg :defaultp
              :index +default-provider-index+
              :index-reader %find-by-defaultp
              :reader defaultp
              :writer (setf %defaultp))
   (%company :initform nil
             :initarg :company
             :index +company-index+
             :index-reader saml-auth-providers-for-company
             :reader saml-company)
   (name :initarg :name
         :initform "generic-saml"
         :reader saml-name
         :writer (setf %saml-name))
   (expiration-seconds :initarg :expiration-seconds
                       :reader expiration-seconds
                       :initform (* 20 3600))
   (%edited-at :initform nil
               :writer (setf %edited-at)
               :documentation "When the IdP config last changed. Tests from before this
don't count. Use SAML-EDITED-AT, this isn't bound on older providers."))
  (:metaclass persistent-class))

(defmethod saml-idp-metadata-url ((self saml-auth-provider))
  "NIL for providers created with only METADATA-XML (e.g. at the REPL)."
  (when (slot-boundp self 'idp-metadata-url)
    (idp-metadata-url self)))

(defmethod saml-edited-at ((self saml-auth-provider))
  (when (slot-boundp self '%edited-at)
    (slot-value self '%edited-at)))

(defun edit-saml-provider (provider &key name idp-metadata-url)
  "Change PROVIDER's NAME and IdP metadata URL. Changing the URL of the
active provider isn't allowed: it would change everyone's login without
a test. Create a new provider and activate it instead."
  (bt:with-lock-held (*lock*)
    (let ((url-changed-p (not (equal idp-metadata-url
                                     (saml-idp-metadata-url provider)))))
      (when (and url-changed-p (defaultp provider))
        (error "Can't change the IdP of the active SAML provider"))
      (with-transaction ()
        (setf (%saml-name provider) name)
        (when url-changed-p
          (setf (%idp-metadata-url provider) idp-metadata-url)
          ;; Otherwise we'd keep using the old IdP's metadata.
          (setf (%metadata-xml provider) nil)
          (setf (%edited-at provider) (get-universal-time)))))))

(defun default-saml-auth-provider ()
  (%find-by-defaultp t))

(defparameter *activation-test-window* (* 60 60)
  "How recent the test must be when activating a provider.")

(defun activation-blocker (provider user)
  "NIL if USER may make PROVIDER the default login for this installation,
otherwise a message explaining why not.

Activating replaces the login for everyone, so we require a recent
successful test by USER.

We don't require the test to sign in as a company admin, though without
one nobody can administer the installation after activating. A
recovery token gets back to the SSO page in that case."
  (let ((result (last-saml-test-result provider)))
    (cond
      ((typep *installation* 'multi-org-feature)
       ;; DEFAULTP applies to the whole installation, it would make
       ;; this company's IdP everyone's login.
       "Activating SAML is only supported on single-company installations")
      ((not (eql (saml-company provider)
                 (get-singleton-company *installation*)))
       "This provider doesn't belong to this installation's company")
      ((defaultp provider)
       "This provider is already active")
      ((null result)
       "Test this provider before activating it")
      ((saml-test-result-error result)
       "The last test failed")
      ((not (eql user (saml-test-result-tested-by result)))
       "Test this provider yourself before activating it")
      ((< (saml-test-result-ts result)
          (- (get-universal-time) *activation-test-window*))
       "The last test is more than an hour old, test it again")
      ((and (saml-edited-at provider)
            ;; <=: in the same second we can't tell which came first
            (<= (saml-test-result-ts result)
                (saml-edited-at provider)))
       "The provider changed since the last test, test it again")
      (t
       nil))))

(defun activate-saml-provider (provider user)
  "Make PROVIDER the default login for this installation, replacing the
current default if there is one."
  (bt:with-lock-held (*lock*)
    (let ((blocker (activation-blocker provider user)))
      (when blocker
        (error "Can't activate SAML provider: ~a" blocker)))
    (let ((old (default-saml-auth-provider)))
      (with-transaction ()
        ;; Clear the old one first, %DEFAULTP has a unique index.
        (when old
          (setf (%defaultp old) nil))
        (setf (%defaultp provider) t)))))

(defun deactivate-saml-provider (provider)
  (with-transaction ()
    (setf (%defaultp provider) nil)))

(defun delete-saml-provider (provider)
  "Delete PROVIDER and its test results. The active provider can't be
deleted, since that would lock everyone out: deactivate it, or activate
another provider, first."
  (bt:with-lock-held (*lock*)
    (when (defaultp provider)
      (error "Can't delete the active SAML provider"))
    (fset:do-set (result (saml-test-results-for-provider provider))
      (bknr.datastore:delete-object result))
    (bknr.datastore:delete-object provider)))

(defmethod saml-entity-id ((self saml-auth-provider))
  (handler-case
      (get-idp-entity-id
       (create-settings-builder-for-xml
        (saml-company self)
        (metadata-xml self)))
    (invalid-metadata-url ()
      "Error loading metadata")))

(defmethod metadata-xml ((self saml-auth-provider))
  (util/misc:or-setf
   (%metadata-xml self)
   (fetch-metadata-xml (idp-metadata-url self))))

(defmethod saml-external-request-engine (installation)
  (warn "Using default request engine to request SAML metadata")
  *engine*)

(defun fetch-metadata-xml (url)
  (http-request
   url
   :want-string t
   :engine (saml-external-request-engine *installation*)))

(lw-ji:define-java-constructor make-saml-response "com.onelogin.saml2.authn.SamlResponse")

(lw-ji:define-java-callers "com.onelogin.saml2.authn.SamlResponse"
  (saml-response-is-valid "isValid")
  (saml-response-get-error "getError")
  (saml-response-get-attributes "getAttributes")
  (saml-response-get-xml "getSAMLResponseXml")
  (saml-response-get-name-id "getNameId")
  (saml-response-get-response-issuer "getResponseIssuer"))

(defun current-request-uri ()
  (quri:render-uri
   (quri:make-uri
    :scheme (or
             (hunchentoot:header-in* "x-forwarded-proto")
             (hunchentoot:server-protocol*))
    :host (first (str:split ":" (hunchentoot:host)))
    :path (hunchentoot:request-uri*))))

(defun find-saml-auth-provider-for-response (saml-response request-uri)
  "Find the provider whose IdP entity ID matches the Issuer of
SAML-RESPONSE. The Issuer is not trusted at this point, it only picks
which provider's certificate we validate the response against."
  (loop for provider in (bknr.datastore:class-instances 'saml-auth-provider)
        for match = (handler-case
                        (let* ((settings (create-settings-builder-for-xml
                                          (saml-company provider)
                                          (metadata-xml provider)))
                               (resp (make-saml-response settings
                                                         request-uri
                                                         saml-response)))
                          (equal (saml-response-get-response-issuer resp)
                                 (get-idp-entity-id settings)))
                      (error (e)
                        (log:warn "Skipping SAML provider ~a: ~a" provider e)
                        nil))
        if match
          return provider))

(defhandler (nil :uri "/sso/saml/callback") ()
  (handler-case
      (let* ((saml-response (hunchentoot:post-parameter "SAMLResponse"))
             (relay-state-id (hunchentoot:post-parameter "RelayState"))
             (relay-state (or
                           (relay-state-for-id relay-state-id)
                           ;; IdP-initiated login: guess the provider
                           ;; from the response's Issuer.
                           (make-relay-state
                            (or
                             (find-saml-auth-provider-for-response
                              saml-response
                              (current-request-uri))
                             (error "Could not find a SAML provider for this response")))))
             (request-uri (current-request-uri)))
        (handle-saml-response
         (make-saml-response (relay-state-settings relay-state)
                             request-uri
                             saml-response)
         relay-state))
    (relay-state-expired ()
      <html>
        <p>This SAML session has expired, probably because of a rogue refresh. <a href= "/">Try again.</a></p>
      </html>)))

(lw-ji:define-java-callers "java.util.List"
  (list-get "get"))


(defun handle-saml-response (resp relay-state)
  (when (bknr.indices:object-destroyed-p (saml-auth-provider relay-state))
    ;; The provider was deleted after this login started.
    (error "This SAML connection was deleted"))
  (let ((is-valid (saml-response-is-valid resp)))
    (cond
      ((relay-state-tested-by relay-state)
       (finish-test-login
        relay-state
        :email (when is-valid
                 (saml-response-get-name-id resp))
        :error-message (unless is-valid
                         (or (saml-response-get-error resp)
                             "Unknown error"))))
      ((not is-valid)
       (error "SamlResponse failed to validate: ~a [~a]"
              (saml-response-get-error resp)
              is-valid))
      (t
       (process-validated-callback resp relay-state)))))

(defun finish-test-login (relay-state &key email error-message)
  "Record the result of a test login. This must not touch the session:
the POST from the IdP doesn't carry the session cookie (SameSite=Lax),
so creating a session here would log the admin out."
  (make-instance 'saml-test-result
                 :provider (saml-auth-provider relay-state)
                 :tested-by (relay-state-tested-by relay-state)
                 :email email
                 :error-message error-message)
  (hex:safe-redirect
   (or
    (redirect relay-state)
    "/")))

(defun process-validated-callback (saml-response relay-state)
  (let* ((attributes (saml-response-get-attributes saml-response))
         (entity-id (get-idp-entity-id (relay-state-settings relay-state)))
         (email (saml-response-get-name-id saml-response))
         (saml (saml-auth-provider relay-state)))
    (declare (ignore attributes entity-id))
    (when (site-admin-sso-forbidden-p email)
      (return-from process-validated-callback
        <html>
          <p>Site admins can't sign in with SAML on this installation. <a href= "/">Go back.</a></p>
        </html>))
    (let ((user (find-user-for-sso (saml-company saml) email )))
      ;; TODO: this is not enough. We need to restrict the login to
      ;; only this company.
      (setf (auth:current-user :expires-in (expiration-seconds saml))
            user)
      ;; During the POST, we lose the original session because of
      ;; SameSite=Lax. This allows us to still call nibbles from that
      ;; session.
      (auth:make-session-reset
       :old-token (original-session-token relay-state)
       :new-token (auth::%session-token (auth:current-session)))
      (setf (auth:session-value :sso-company) (saml-company saml))
      (hex:safe-redirect
       (or
        (redirect relay-state)
        "/runs")))))


(defun site-admin-sso-forbidden-p (email)
  "Check before FIND-USER-FOR-SSO, so we don't touch the site admin's
user. See SITE-ADMINS-CAN-USE-SSO-P."
  (let ((user (auth:find-user *installation* :email email)))
    (and user
         (adminp user)
         (not (site-admins-can-use-sso-p *installation*)))))

(defclass saml-user (single-company-user)
  ()
  (:metaclass persistent-class)
  (:documentation "deprecated: we use regular users now. This class didn't exist for too long"))

(defmethod find-user-for-sso ((company company) email)
  (bt:with-lock-held (*lock*)
    ;; TODO: If this is self-service, we need domain trust, but that's
    ;; for another diff.
    (let ((user (auth:find-or-create-user *installation* :email email)))
      (roles:ensure-has-role company user 'roles:standard-member)
      user)))


(lw-ji:define-java-callers "com.onelogin.saml2.settings.IdPMetadataParser"
  (parse-file-xml "parseFileXML")
  (parse-remote-xml "parseRemoteXML"))

(lw-ji:define-java-constructor  make-java-url "java.net.URL")

(lw-ji:define-java-callers "java.util.Map"
  (java-map-get "get")
  (java-map-put "put"))

(lw-ji:define-java-constructor make-settings-builder "com.onelogin.saml2.settings.SettingsBuilder")

(lw-ji:define-java-callers "com.onelogin.saml2.settings.SettingsBuilder"
  (settings-builder-from-values "fromValues")
  (settings-builder-build "build"))

(lw-ji:define-java-callers "com.onelogin.saml2.settings.Saml2Settings"
  (settings-get-signature-algorithm "getSignatureAlgorithm")
  (get-sp-key "getSPkey")
  (get-sp-metadata "getSPMetadata")
  (get-idp-entity-id "getIdpEntityId"))

(lw-ji:define-java-callers "com.onelogin.saml2.util.Util"
  (util-sign "sign")
  (util-base64-encoder "base64encoder"))


(lw-ji:define-java-constructor make-hash-map "java.util.HashMap")

(lw-ji:define-java-constructor make-authn-request "com.onelogin.saml2.authn.AuthnRequest")

(lw-ji:define-java-callers "com.onelogin.saml2.authn.AuthnRequest"
  (authn-request-get-encoded-authn-request "getEncodedAuthnRequest"))


(define-condition invalid-metadata-url (error)
  ())

(defun get-idp-settings (xml)
  (uiop:with-temporary-file (:pathname p :stream s :direction :output :type "xml")
    (write-string xml s)
    (finish-output s)
    (handler-case
        (parse-remote-xml (make-java-url (format nil "file://~a" (namestring p))))
      (lw-ji:java-method-exception (e)
        (log:error "Got java error: ~a" e)
        (error 'invalid-metadata-url)))))

(defun parse-xml (xml)
  (java-map-get
   (get-idp-settings xml)
   "onelogin.saml2.idp.single_sign_on_service.url"))

(defun entity-id ()
  "Our SP entity ID, which IdPs use as the Audience URI.

Don't change this: customers type it into their IdP by hand (e.g. Okta's
\"Audience URI\"), and at least one customer is already configured
with <domain>/saml/metadata. Changing it would break their
logins with an audience mismatch. It's also served as our metadata on
single-company installations, see SINGLETON-SP-METADATA."
  (let ((uri (quri:uri
              (installation-domain
               *installation*))))
    (setf (quri:uri-path uri)
          "/saml/metadata")
    (quri:render-uri uri)))

(defun callback-url ()
  "The Assertion Consumer Service URL (Okta's \"Single sign-on URL\")."
  (quri:render-uri
   (quri:make-uri
    :path "/sso/saml/callback"
    :defaults (quri:uri (installation-domain *installation*)))))

(defun create-settings-builder-for-xml (company xml)
  (let ((settings-map (cond
                        (xml
                         (make-hash-map (get-idp-settings xml)))
                        (t
                         (make-hash-map)))))
    (java-map-put settings-map "onelogin.saml2.sp.entityid" (entity-id))
    (java-map-put settings-map "onelogin.saml2.sp.assertion_consumer_service.url"
                  (callback-url))
    (java-map-put settings-map "onelogin.saml2.sp.x509cert"
                  (saml-public-cert
                   (find-or-create-saml-cert company)))
    (java-map-put settings-map "onelogin.saml2.sp.privatekey"
                  (saml-private-key
                   (find-or-create-saml-cert company)))
    (java-map-put settings-map "onelogin.saml2.sp.nameidformat"
                  "urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress")
    (loop for key in '("onelogin.saml2.security.authnrequest_signed"
                       "onelogin.saml2.security.logoutrequest_signed"
                       "onelogin.saml2.security.sign_metadata"
                       "onelogin.saml2.security.logoutresponse_signed"
                       "onelogin.saml2.security.allow_duplicated_attribute_name")
          do 
             (java-map-put settings-map key
                           "true"))
    (settings-builder-build
     (settings-builder-from-values
      (make-settings-builder)
      settings-map))))


#+nil
(parse-xml "<md:EntityDescriptor xmlns='urn:oasis:names:tc:SAML:2.0:metadata' xmlns:md='urn:oasis:names:tc:SAML:2.0:metadata' xmlns:saml='urn:oasis:names:tc:SAML:2.0:assertion' xmlns:ds='http://www.w3.org/2000/09/xmldsig#' entityID='http://localhost:8080/realms/master'><md:IDPSSODescriptor WantAuthnRequestsSigned='true' protocolSupportEnumeration='urn:oasis:names:tc:SAML:2.0:protocol'><md:KeyDescriptor use='signing'><ds:KeyInfo><ds:KeyName>KGh_gBNYNUgpON0_vz_AjVwJhAcRGZSSwWELSD6Al-Q</ds:KeyName><ds:X509Data><ds:X509Certificate>MIICmzCCAYMCBgGeV3VXnzANBgkqhkiG9w0BAQsFADARMQ8wDQYDVQQDDAZtYXN0ZXIwHhcNMjYwNTI0MDA0NzQ4WhcNMzYwNTI0MDA0OTI4WjARMQ8wDQYDVQQDDAZtYXN0ZXIwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQC1BzweaB4nAZ903zJXA0EMTi/vhq0LUwHM+8hiJXSsZ0IzKCCkoJymS0txSirRekiItw31/YV9kZhWY0OsQM9YmrCoq4HeOgA/B4Xy60zTzalEIlleRIYfQPdU3X7795LY2Ajgxu80AwAj62yP9VpDwaAaVYD9LECwNGL9Z6WGoSrI1NSHzFbaVAKMezrnHEhiJ5MZjjNCIaMU/KOlkUCEBofO7jMsaiER4IkSJhEH8UfQueHr3Vneu8O7n1yfg0CyNLZHF4NnDioPpIFt+O1yvCtEVuDqVoGkqn0JwajPlhOwX6Nrfe0u/MrSEiiwmUXY5droAve2CZW7T3GV1lzLAgMBAAEwDQYJKoZIhvcNAQELBQADggEBACeVAiUCvWzblxFCXgR6Lt/MAIXiQad5O4hzvwfq1d/QmZcd1f63jNbMEY+LKQhdd8A+zszKRsckJ6HcuK17EhEgFA2QM8aBrMohs13LDNoV7KhbsVQRy2wVwcEsWtoolf78eGEN7ydHpvNkBS3X+MUm34miEoUetMg4cTi8fQ0OixjZy/4O/01qa2j3Mebbrip8hk38CA4ltjvsoZzw54Y162WUqy3/jeLbhcRTO8GmcDkJlVpqSu/IEaC5T0jvw5v/5ZMlEFpJtIn0AYbyW0Ck47FiKU6FukkXrVlbfvgVLroa2IDwytXkBhPMer7LM+g2CoZc1tUx38JTqQ9nkSY=</ds:X509Certificate></ds:X509Data></ds:KeyInfo></md:KeyDescriptor><md:ArtifactResolutionService Binding='urn:oasis:names:tc:SAML:2.0:bindings:SOAP' Location='http://localhost:8080/realms/master/protocol/saml/resolve' index='0'></md:ArtifactResolutionService><md:SingleLogoutService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleLogoutService><md:SingleLogoutService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleLogoutService><md:SingleLogoutService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Artifact' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleLogoutService><md:SingleLogoutService Binding='urn:oasis:names:tc:SAML:2.0:bindings:SOAP' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleLogoutService><md:NameIDFormat>urn:oasis:names:tc:SAML:2.0:nameid-format:persistent</md:NameIDFormat><md:NameIDFormat>urn:oasis:names:tc:SAML:2.0:nameid-format:transient</md:NameIDFormat><md:NameIDFormat>urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified</md:NameIDFormat><md:NameIDFormat>urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress</md:NameIDFormat><md:SingleSignOnService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleSignOnService><md:SingleSignOnService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleSignOnService><md:SingleSignOnService Binding='urn:oasis:names:tc:SAML:2.0:bindings:SOAP' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleSignOnService><md:SingleSignOnService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Artifact' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleSignOnService></md:IDPSSODescriptor></md:EntityDescriptor>")

(defun build-request-signature (arg relay-state sig-alg
                                key)
  (let ((uri (quri:uri "")))
    (setf
     (quri:uri-query-params uri)
     `(("SAMLRequest" . ,arg)
       ("RelayState" . ,relay-state)
       ("SigAlg" . ,sig-alg)))
    (util-base64-encoder
     (util-sign (quri:uri-query uri)
                key
                sig-alg))))

(defmethod make-relay-state ((self saml-auth-provider) &rest args &key &allow-other-keys)
  (let* ((xml (metadata-xml self))
         (url (quri:uri (parse-xml xml)))
         (settings (create-settings-builder-for-xml (saml-company self) xml)))
    (values
     (apply #'make-instance 'relay-state
            :saml-auth-provider self
            :settings settings
            :original-session-token (auth:%session-token (auth:current-session))
            args)
     settings
     url)))


(defmethod signin-link ((self saml-auth-provider) redirect)
  (%signin-link self :redirect redirect))

(defun test-signin-link (self tested-by redirect)
  "Like SIGNIN-LINK, but the callback only records a SAML-TEST-RESULT
and then redirects to REDIRECT, without logging anyone in."
  (check-type tested-by store-object)
  (%signin-link self :redirect redirect :tested-by tested-by))

(defun %signin-link (self &rest relay-state-args)
  (multiple-value-bind (relay-state settings url)
      (apply #'make-relay-state self relay-state-args)
   (let* ((arg (authn-request-get-encoded-authn-request
                (make-authn-request settings)))
          (sig-alg (settings-get-signature-algorithm settings))
          (signature (build-request-signature
                      arg
                      (relay-state-id relay-state)
                      sig-alg
                      (get-sp-key settings))))
     (setf
      (quri:uri-query-params url)
      `(("SAMLRequest" . ,arg)
        ("RelayState" . ,(relay-state-id relay-state))
        ("SigAlg" . ,sig-alg)
        ("Signature" . ,signature)))
     (quri:render-uri url))))

(defmethod logo-svg ((self saml-auth-provider))
  nil)

(defmethod auth-provider-signin-form ((self saml-auth-provider) redirect)
  <div class= "form-group mt-1 text-center mb-0">
    <a class= "btn btn-outline-secondary" style= "width:100%"  href= (signin-link self redirect) >
      ,(logo-svg self)
      <span class= "ms-1">Sign in with ,(saml-name self) </span>
    </a>
  </div>)

(defhandler (nil :uri "/saml/:oid/login") (oid redirect)
  (let ((saml (find-by-oid oid)))
    (check-type saml saml-auth-provider)
    (hex:safe-redirect (signin-link saml (or redirect "/runs")))))

(defun render-sp-metadata (company)
  (check-type company company)
  (setf (hunchentoot:content-type*) "application/xml")
  (get-sp-metadata
   (create-settings-builder-for-xml company nil)))

(defhandler (sp-metadata :uri "/saml/entity/:oid/metadata") (oid)
  "The IdP fetches this without a session, so the company must come from
the OID, not from the current session."
  (render-sp-metadata (util:find-by-oid oid)))

(defhandler (singleton-sp-metadata :uri "/saml/metadata") ()
  "The metadata for installations with a single company (e.g. enterprise
installs). This is also the SP entity ID, see ENTITY-ID."
  (cond
    ((typep *installation* 'multi-org-feature)
     (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
     (hunchentoot:abort-request-handler))
    (t
     (render-sp-metadata (get-singleton-company *installation*)))))

(defmethod oauth-signin-link ((self saml-auth-provider) redirect)
  (signin-link self (or redirect "/runs")))


(defmethod logout-link ((self saml-auth-provider))
  "/saml/logout")

(defhandler (nil :uri "/saml/logout") ()
  (setf (auth:current-user) nil)
  <html>
    You've been logged out. <a href= "/">Go back.</a>
  </html>)

(defmethod end-session-endpoint ((self saml-auth-provider))
  "/saml/logout")

(defun clear-relay-states ()
  (map-relay-states
   (lambda (relay-state)
     (when (< (created-at relay-state) (- (get-universal-time) 600))
       (setf (relay-state-id relay-state)  nil)))))

(def-cron clear-old-replay-states ()
  (clear-relay-states))
