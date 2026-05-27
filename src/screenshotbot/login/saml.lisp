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
  (:export
   #:saml-auth-provider))
(in-package :screenshotbot/login/saml)

(named-readtables:in-readtable markup:syntax)

(defindex +id-index+
  'fset-unique-index
  :slot-name 'id)

(defclass relay-state (base-indexed-object)
  ((id :initarg :id
       :index +id-index+
       :index-reader relay-state-for-id
       :reader relay-state-id
       :initform (format nil "~a" (secure-random:number 10000000000000000)))
   (settings :initarg :settings
             :reader relay-state-settings))
  (:metaclass indexed-class))


;; These are hardcoded just for testing purposes, before we productionize this we'll auto-generate it with P176
(defvar *fake-saml-cert*
  "MIIDJzCCAg+gAwIBAgIUboUduNIKHQWmKnEcA4+qxm2j37QwDQYJKoZIhvcNAQELBQAwIzEhMB8GA1UEAwwYc3RhZ2luZy5zY3JlZW5zaG90Ym90LmlvMB4XDTI2MDUyNDE1Mjg0M1oXDTM2MDUyMTE1Mjg0M1owIzEhMB8GA1UEAwwYc3RhZ2luZy5zY3JlZW5zaG90Ym90LmlvMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAnbhcGrFR3vPo+BgjuCae4Eo5lp9tA/0NgEb6lA5fbqRBPIVkno8taZKjtXiwpNpWeNaL4x7sYRgVQZ3BgLPe1WMH9Zt1BH+ZHj5corgHTLsokKbYOvfjhc2I3buH4EpFU4LEwtRQeBNtXu7iygLzn6RJRlDKDHmVCaBos8hohcPqk+RlC1EgQS8XGX/c2yKm5mNMS8FEOb2WGis9lrWn3juyCvBxQM/3Jzj1T4tpH18D0fnM0y3OzkjzaaUkoe780IZVZoMNBFA7TRUrEclcM1a6vY0zwU78yYrQVSZ/K0SfIA/dJJZOe30swEJKVkPp2xPcEUC8+ktzT5JhS1LUvwIDAQABo1MwUTAdBgNVHQ4EFgQUvbijq5U5EHTMsXFcHCYhUCVXHzYwHwYDVR0jBBgwFoAUvbijq5U5EHTMsXFcHCYhUCVXHzYwDwYDVR0TAQH/BAUwAwEB/zANBgkqhkiG9w0BAQsFAAOCAQEAfnQ22i8OXqPb6ptB9g6wKrVxQlC1otPlDd5we7Mu+hL8wlMwXcJGKiI8Mtf3g0puRq1Orl3FFma1e6yigdN2H0kiNpYY5eo9UeqA1Q/+sJTTPFE0COxYZOlddAGalBUWyZN5w9Oz91s6tCGaRvB7hFwHTRLtGRbjJUygjgM/j6LuB+FVX0IGizY5ellTRzxpqqi/fNAjLE7EsmxhzAsxPs8UWoTeLQYwD6kRqV/d+G5bahP250VyHgfChSWBYRBTObI/lX8L+JBX3Rvg3ERWREJSUafbdCe1R+fi1sLu/P60EZyCw9JufX59umdEeTEvOEa/ybCDYAanV+9Gw4DpRg==")

(defvar *fake-saml-key*
  "MIIEvwIBADANBgkqhkiG9w0BAQEFAASCBKkwggSlAgEAAoIBAQCduFwasVHe8+j4GCO4Jp7gSjmWn20D/Q2ARvqUDl9upEE8hWSejy1pkqO1eLCk2lZ41ovjHuxhGBVBncGAs97VYwf1m3UEf5kePlyiuAdMuyiQptg69+OFzYjdu4fgSkVTgsTC1FB4E21e7uLKAvOfpElGUMoMeZUJoGizyGiFw+qT5GULUSBBLxcZf9zbIqbmY0xLwUQ5vZYaKz2WtafeO7IK8HFAz/cnOPVPi2kfXwPR+czTLc7OSPNppSSh7vzQhlVmgw0EUDtNFSsRyVwzVrq9jTPBTvzJitBVJn8rRJ8gD90klk57fSzAQkpWQ+nbE9wRQLz6S3NPkmFLUtS/AgMBAAECggEATAmDIf1FzrqBoQYmRlQcOV6fd+3hZVBc73CIwtNRD+rRZqeatFSrnJ+tHEKUys1WbghlRXh1lnPBX7J6BR3yeqa1QiQR3LrVa36+M1aMcmIystY1Hey/fJTz/I45+hhkZtf/Gzy3lMQs8N0zahfVMyxFhUhSuIPvJcZ3Y+Fk/sOMN6fFw8z/ZTBvZekN0CPC5VsWyeBz9Jn+XG+zfTbRqfm5tlNtJsW6U7B09Ctfr88DnDRiYtNNY32ilnjhG6HKLPU8wYOEPiA9zOMTey/jYaMywAAbSTpMCgS61bFiOJfCiGbDGAVl81KrhjtVSUwUSuMqtNVvUJSTy8BN7/zmAQKBgQDbX3GRNAUB1ztb7Ja/apQCdm4hHkma9zIXyYNEPy/eBt4L53wToaba33oI9duIWY2aUhHrrBlnU29XM+Hx/4wURnx686WuI4ZySTapVvteSC/Zm09oFB+f7QiathNS1n1wML5wOnN5kZhh+Uf8xoO0Y1aIETwHmqFdJzBX66+2AQKBgQC4Dbkd3C1Ko7D4dqB2fN06N7ww95Fz2S1pl/w8V2s4uoWoPqUonj3v2vj51fadAbSWoeQ0TlvUUTtVNSezFgAoWhOxJQKrA+6+uE6Xt7C5sivEHxYvQrj79kOYW6xTZ8AB8/RN66jNJ16unTHZVdcBGRdrDSvpLSegXsYQQh4KvwKBgQCdNpZWAFjCS/QvWatjPMcbyLH+LA2F8DfHElRveXUdggBpuZiTHRtN6jAz8bZFziAMA1rycaC3CvVVIkp/uqsx8J3PI4ON+8mjZ9KzozF8DPG12nca2KkdXKr47RmGGU9GMriYB1uwOOZi+FpdzgqfIT3nP6qsrGWOM8KSj8aaAQKBgQCTkd00xc5CpBBGhsaNefveq8Vl9XlXy2+P1F5W+zhq2ZJEnUXK1WWPpKAvoJAEvtNOWysfjRwvlZne7amQ+zjRIbfcNnJ3L8YCgL/zAULfAK36p3ogFn0+9+qmhAodLXhTmIfu2d4T71cI5dyMBzlGFhoiqQLmCGBXQuXHL1vq/QKBgQCzRM3Pacgsvg1SSv38aqMan/f/AZOs7V+/GzCqEQwLwZilqlx17rsQhB06W6S0dcbprrP5ZfwMRLfRcu9+74lINLzO9Q2dOINwOrKRq02s5dfk1Sf+Um+2Fm2PKfAqEhG1AIduu26JRQXDbPtAQ2+7z6XDtb3IKmOWFl/CNfcEFg==")

(defclass saml-auth-provider (auth-provider
                              store-object)
  ((entity-id :initform "https://screenshotbot.io"
              :initarg :entity-id
              :reader entity-id)
   (idp-metadata-url :initarg :idp-metadata-url
                     :reader idp-metadata-url)
   (metadata-xml :initform nil
                 :initarg :metadata-xml
                 :reader %metadata-xml)
   (%company :initform nil
             :initarg :company
             :reader saml-company)
   (name :initarg :name
         :initform "generic-saml"
         :reader saml-name))
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
         (email (saml-response-get-name-id saml-response)))
    (declare (ignore attributes))
    (error "id is ~a,~a" entity-id email)))

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

(defun create-settings-builder-for-xml (self xml)
  (let ((settings-map (make-hash-map (get-idp-settings xml))))
    (java-map-put settings-map "onelogin.saml2.sp.entityid" (entity-id self))
    (java-map-put settings-map "onelogin.saml2.sp.assertion_consumer_service.url"
                  ;; todo
                  "https://staging.screenshotbot.io/sso/saml/callback")
    (java-map-put settings-map "onelogin.saml2.sp.x509cert"
                  *fake-saml-cert*)
    (java-map-put settings-map "onelogin.saml2.sp.privatekey"
                  *fake-saml-key*)
    (java-map-put settings-map "onelogin.saml2.sp.nameidformat"
                  "urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress")
    (loop for key in '("onelogin.saml2.security.authnrequest_signed"
                       "onelogin.saml2.security.logoutrequest_signed"
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
           (settings (create-settings-builder-for-xml self xml))
           (relay-state (make-instance 'relay-state
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

(defhandler (nil :uri "/saml/:id/login") (id redirect)
  (let ((saml (bknr.datastore:store-object-with-id (parse-integer id))))
    (check-type saml saml-auth-provider)
    (hex:safe-redirect (signin-link saml (or redirect "/runs")))))
