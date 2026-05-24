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
  (:export
   #:saml-auth-provider))
(in-package :screenshotbot/login/saml)

(named-readtables:in-readtable markup:syntax)

(defclass saml-auth-provider (auth-provider
                              roles-auth-provider)
  ((entity-id :initform "https://screenshotbot.io"
              :initarg :entity-id
              :reader entity-id)
   (idp-metadata-url :initarg :idp-metadata-url
                     :reader idp-metadata-url)
   (metadata-xml :initform nil
                 :initarg :metadata-xml
                 :reader %metadata-xml)
   (name :initarg :name
         :initform "generic-saml"
         :reader saml-name)))

(defmethod metadata-xml ((self saml-auth-provider))
  (or
   (%metadata-xml self)
   (http-request
    (idp-metadata-url self)
    :want-string t)))

(defhandler (nil :uri "/sso/saml/callback") ()
  (error "unimpl"))

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
    (settings-builder-build
     (settings-builder-from-values
      (make-settings-builder)
      settings-map))))

(defun create-authn-request (self xml)
  (make-authn-request (create-settings-builder-for-xml self xml)))

#+nil
(parse-xml "<md:EntityDescriptor xmlns='urn:oasis:names:tc:SAML:2.0:metadata' xmlns:md='urn:oasis:names:tc:SAML:2.0:metadata' xmlns:saml='urn:oasis:names:tc:SAML:2.0:assertion' xmlns:ds='http://www.w3.org/2000/09/xmldsig#' entityID='http://localhost:8080/realms/master'><md:IDPSSODescriptor WantAuthnRequestsSigned='true' protocolSupportEnumeration='urn:oasis:names:tc:SAML:2.0:protocol'><md:KeyDescriptor use='signing'><ds:KeyInfo><ds:KeyName>KGh_gBNYNUgpON0_vz_AjVwJhAcRGZSSwWELSD6Al-Q</ds:KeyName><ds:X509Data><ds:X509Certificate>MIICmzCCAYMCBgGeV3VXnzANBgkqhkiG9w0BAQsFADARMQ8wDQYDVQQDDAZtYXN0ZXIwHhcNMjYwNTI0MDA0NzQ4WhcNMzYwNTI0MDA0OTI4WjARMQ8wDQYDVQQDDAZtYXN0ZXIwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQC1BzweaB4nAZ903zJXA0EMTi/vhq0LUwHM+8hiJXSsZ0IzKCCkoJymS0txSirRekiItw31/YV9kZhWY0OsQM9YmrCoq4HeOgA/B4Xy60zTzalEIlleRIYfQPdU3X7795LY2Ajgxu80AwAj62yP9VpDwaAaVYD9LECwNGL9Z6WGoSrI1NSHzFbaVAKMezrnHEhiJ5MZjjNCIaMU/KOlkUCEBofO7jMsaiER4IkSJhEH8UfQueHr3Vneu8O7n1yfg0CyNLZHF4NnDioPpIFt+O1yvCtEVuDqVoGkqn0JwajPlhOwX6Nrfe0u/MrSEiiwmUXY5droAve2CZW7T3GV1lzLAgMBAAEwDQYJKoZIhvcNAQELBQADggEBACeVAiUCvWzblxFCXgR6Lt/MAIXiQad5O4hzvwfq1d/QmZcd1f63jNbMEY+LKQhdd8A+zszKRsckJ6HcuK17EhEgFA2QM8aBrMohs13LDNoV7KhbsVQRy2wVwcEsWtoolf78eGEN7ydHpvNkBS3X+MUm34miEoUetMg4cTi8fQ0OixjZy/4O/01qa2j3Mebbrip8hk38CA4ltjvsoZzw54Y162WUqy3/jeLbhcRTO8GmcDkJlVpqSu/IEaC5T0jvw5v/5ZMlEFpJtIn0AYbyW0Ck47FiKU6FukkXrVlbfvgVLroa2IDwytXkBhPMer7LM+g2CoZc1tUx38JTqQ9nkSY=</ds:X509Certificate></ds:X509Data></ds:KeyInfo></md:KeyDescriptor><md:ArtifactResolutionService Binding='urn:oasis:names:tc:SAML:2.0:bindings:SOAP' Location='http://localhost:8080/realms/master/protocol/saml/resolve' index='0'></md:ArtifactResolutionService><md:SingleLogoutService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleLogoutService><md:SingleLogoutService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleLogoutService><md:SingleLogoutService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Artifact' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleLogoutService><md:SingleLogoutService Binding='urn:oasis:names:tc:SAML:2.0:bindings:SOAP' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleLogoutService><md:NameIDFormat>urn:oasis:names:tc:SAML:2.0:nameid-format:persistent</md:NameIDFormat><md:NameIDFormat>urn:oasis:names:tc:SAML:2.0:nameid-format:transient</md:NameIDFormat><md:NameIDFormat>urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified</md:NameIDFormat><md:NameIDFormat>urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress</md:NameIDFormat><md:SingleSignOnService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleSignOnService><md:SingleSignOnService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleSignOnService><md:SingleSignOnService Binding='urn:oasis:names:tc:SAML:2.0:bindings:SOAP' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleSignOnService><md:SingleSignOnService Binding='urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Artifact' Location='http://localhost:8080/realms/master/protocol/saml'></md:SingleSignOnService></md:IDPSSODescriptor></md:EntityDescriptor>")

(defmethod signin-link ((self saml-auth-provider) redirect)
  (let ((xml (metadata-xml self)))
   (let ((url (quri:uri (parse-xml xml)))
         (arg (authn-request-get-encoded-authn-request
               (create-authn-request self xml))))
     (setf
      (quri:uri-query-params url)
      `(("SAMLRequest" . ,arg)))
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
