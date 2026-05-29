;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/saml
  (:use #:cl)
  (:import-from #:core/installation/auth-provider
                #:auth-provider-signin-form
                #:auth-provider)
  (:import-from #:auth/login/roles-auth-provider
                #:roles-auth-provider)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:bknr.indices
                #:indexed-class
                #:base-indexed-object)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-unique-index)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/user
                #:single-company-user)
  (:import-from #:core/installation/installation
                #:*installation*
                #:installation-domain)
  (:import-from #:util/store/object-id
                #:find-by-oid
                #:object-with-oid)
  (:export
   #:saml-auth-provider))
(in-package :screenshotbot/login/saml)

(named-readtables:in-readtable markup:syntax)

(defvar *lock* (bt:make-lock))

(defindex +id-index+
  'fset-unique-index
  :slot-name 'id)

(defclass saml-cert (store-object)
  ((private-key :initarg :private-key
                :reader saml-private-key)
   (cert :initarg :public-cert
         :reader saml-public-cert))
  (:metaclass persistent-class)
  (:documentation "The singleton private-key/cert used for SAML"))

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

(defun make-new-saml-cert ()
  (multiple-value-bind (cert key)
      (make-cert-pair)
    (make-instance 'saml-cert
                   :public-cert cert
                   :private-key key)))

(defun find-or-create-saml-cert ()
  (bt:with-lock-held (*lock*)
    (or
     (first
      (bknr.datastore:store-objects-with-class 'saml-cert))
     (make-new-saml-cert))))

(defclass relay-state (base-indexed-object)
  ((id :initarg :id
       :index +id-index+
       :index-reader relay-state-for-id
       :reader relay-state-id
       :initform (format nil "~a" (secure-random:number 10000000000000000)))
   (saml-auth-provider :initarg :saml-auth-provider
                       :reader saml-auth-provider)
   (settings :initarg :settings
             :reader relay-state-settings))
  (:metaclass indexed-class))


(defclass saml-auth-provider (auth-provider
                              object-with-oid)
  ((idp-metadata-url :initarg :idp-metadata-url
                     :reader idp-metadata-url)
   (metadata-xml :initform nil
                 :initarg :metadata-xml
                 :reader %metadata-xml)
   (%company :initform nil
             :initarg :company
             :reader saml-company)
   (name :initarg :name
         :initform "generic-saml"
         :reader saml-name)
   (expiration-seconds :initarg :expiration-seconds
                       :reader expiration-seconds
                       :initform (* 20 3600)))
  (:metaclass persistent-class))

(defmethod metadata-xml ((self saml-auth-provider))
  (or
   (%metadata-xml self)
   (http-request
    (idp-metadata-url self)
    :want-string t)))

(lw-ji:define-java-constructor make-saml-response "com.onelogin.saml2.authn.SamlResponse")

(lw-ji:define-java-callers "com.onelogin.saml2.authn.SamlResponse"
  (saml-response-is-valid "isValid")
  (saml-response-get-error "getError")
  (saml-response-get-attributes "getAttributes")
  (saml-response-get-xml "getSAMLResponseXml")
  (saml-response-get-name-id "getNameId"))

(defun current-request-uri ()
  (quri:render-uri
   (quri:make-uri
    :scheme (or
             (hunchentoot:header-in* "x-forwarded-proto")
             (hunchentoot:server-protocol*))
    :host (first (str:split ":" (hunchentoot:host)))
    :path (hunchentoot:request-uri*))))

(defhandler (nil :uri "/sso/saml/callback") ()
  (let* ((saml-response (hunchentoot:post-parameter "SAMLResponse"))
         (relay-state-id (hunchentoot:post-parameter "RelayState"))
         (relay-state (relay-state-for-id relay-state-id))
         (request-uri (current-request-uri)))
    (let ((resp (make-saml-response (relay-state-settings relay-state)
                                    request-uri
                                    saml-response )))
      (let ((is-valid (saml-response-is-valid resp)))
        (unless is-valid
          (error "SamlResponse failed to validate: ~a [~a]"
                 (saml-response-get-error resp)
                 is-valid)))
      (process-validated-callback resp relay-state))))

(lw-ji:define-java-callers "java.util.List"
  (list-get "get"))


(defun process-validated-callback (saml-response relay-state)
  (let* ((attributes (saml-response-get-attributes saml-response))
         (entity-id (get-idp-entity-id (relay-state-settings relay-state)))
         (email (saml-response-get-name-id saml-response))
         (saml (saml-auth-provider relay-state)))
    (declare (ignore attributes entity-id))
    (let ((user (find-user-for-sso (saml-company saml) email )))
      ;; TODO: this is not enough. We need to restrict the login to
      ;; only this company.
      (setf (auth:current-user :expires-in (expiration-seconds saml))
            user)
      (setf (auth:session-value :sso-company) (saml-company saml))
      (hex:safe-redirect "/runs"))))


(defclass saml-user (single-company-user)
  ()
  (:metaclass persistent-class))

(defmethod find-user-for-sso ((company company) email)
  (bt:with-lock-held (*lock*)
    (loop for user in (roles:users-for-company company)
          if (string-equal (auth:user-email user) email)
            return user
          finally
          (let ((user (make-instance 'saml-user
                                     :company company
                                     :email email)))
            (roles:ensure-has-role company user 'roles:standard-member)
            (return user)))))

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


(defun get-idp-settings (xml)
  (uiop:with-temporary-file (:pathname p :stream s :direction :output :type "xml")
    (write-string xml s)
    (finish-output s)
    (parse-remote-xml (make-java-url (format nil "file://~a" (namestring p))))))

(defun parse-xml (xml)
  (java-map-get
   (get-idp-settings xml)
   "onelogin.saml2.idp.single_sign_on_service.url"))

(defun entity-id ()
  (let ((uri (quri:uri
              (installation-domain
               *installation*))))
    (setf (quri:uri-path uri)
          "/saml/metadata")
    (quri:render-uri uri)))


(defun create-settings-builder-for-xml (xml)
  (let ((settings-map (cond
                        (xml
                         (make-hash-map (get-idp-settings xml)))
                        (t
                         (make-hash-map)))))
    (java-map-put settings-map "onelogin.saml2.sp.entityid" (entity-id))
    (java-map-put settings-map "onelogin.saml2.sp.assertion_consumer_service.url"
                  ;; todo
                  "https://staging.screenshotbot.io/sso/saml/callback")
    (java-map-put settings-map "onelogin.saml2.sp.x509cert"
                  (saml-public-cert
                   (find-or-create-saml-cert)))
    (java-map-put settings-map "onelogin.saml2.sp.privatekey"
                  (saml-private-key
                   (find-or-create-saml-cert)))
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


(defmethod signin-link ((self saml-auth-provider) redirect)
  (let ((xml (metadata-xml self)))
    (let* ((url (quri:uri (parse-xml xml)))
           (settings (create-settings-builder-for-xml xml))
           (relay-state (make-instance 'relay-state
                                       :saml-auth-provider self
                                       :settings settings))
           (arg (authn-request-get-encoded-authn-request
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

(defhandler (nil :uri "/saml/metadata") ()
  (setf (hunchentoot:content-type*) "application/xml")
  (let ((settings (create-settings-builder-for-xml nil)))
    (get-sp-metadata settings)))
