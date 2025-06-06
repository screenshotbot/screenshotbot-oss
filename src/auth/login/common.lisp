;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/common
  (:use :cl)
  (:nicknames :auth/login/common)
  (:import-from #:auth
                #:can-view!)
  (:import-from #:core/installation/auth
                #:call-with-ensure-user-prepared
                #:company-for-request)
  (:import-from #:core/installation/auth-provider
                #:auth-provider
                #:auth-provider-signin-form
                #:auth-provider-signup-form
                #:call-with-company-login
                #:company-sso-auth-provider)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:nibble
                #:nibble
                #:nibble-id)
  (:import-from #:util/throttler
                #:throttle!
                #:throttler)
  (:import-from #:auth/login/roles-auth-provider
                #:roles-auth-provider)
  (:import-from #:util.cdn
                #:make-cdn)
  (:export
   #:abstract-oauth-provider
   #:after-create-user
   #:make-redirect-nibble
   #:oauth-callback
   #:oauth-logo-svg
   #:oauth-name
   #:oauth-signin-link
   #:oauth-signup-link
   #:signin-get
   #:signup-get
   #:with-oauth-state-and-redirect
   #:server-with-login
   #:auth-template-impl
   #:standard-auth-provider
   #:with-login
   #:or-divider))
(in-package :screenshotbot/login/common)

(named-readtables:in-readtable markup:syntax)

(defclass abstract-oauth-provider (auth-provider
                                   roles-auth-provider)
  ((oauth-name :initarg :oauth-name
               :accessor oauth-name)))

(defmethod auth-provider-signin-form ((auth-provider abstract-oauth-provider) redirect)
    <div class= "form-group mt-1 text-center mb-0">
      <a class= "btn btn-outline-secondary" style= "width:100%"  href= (oauth-signin-link auth-provider redirect) >
        ,(oauth-logo-svg auth-provider)
        <span class= "ms-1">Sign in with ,(oauth-name auth-provider) </span>
      </a>
    </div>)

(defgeneric after-create-user (installation user)
  (:method (installation user)
    (declare (ignore installation user))
    (values)))

(markup:deftag or-divider ()
  <div class= "or-wrapper row" >
    <div class= "col-5 strikethrough" >
      <hr />
    </div>
    <div class= "col-2 align-middle">
      <div class= "text" >
        <span class= "align-middle" >
          <em>or</em>
        </span>
      </div>
    </div>
    <div class= "col-5 strikethrough" >
      <hr />
    </div>
  </div>)

(markup:deftag auth-common-header (children)
  <div class="text-center">
    <a href= "/"><img src= (make-cdn "/assets/images/logo-dark.svg") class= "auth-small-logo mb-3" /></a>
    <p class="text-muted mt-3 font-weight-bold">,@(progn children) </p>
  </div>
)

(defmethod auth-provider-signup-form ((auth-provider abstract-oauth-provider)
                                      invite
                                      plan
                                      redirect)
    <div class= "form-group mt-3 text-center mb-3">
      <a class= "btn btn-outline-secondary" style= "width:100%"  href= (oauth-signup-link auth-provider redirect) >
        ,(oauth-logo-svg auth-provider)
        <span class= "ms-1">Sign up with ,(oauth-name auth-provider) </span>
      </a>
    </div>)

(defun server-with-login (fn &key needs-login signup alert company
                           ;; The invite object that triggered this
                           ;; flow.
                           invite
                           ;; Redirect a GET request back to the
                           ;; original URL instead of a nibble.
                           (allow-url-redirect nil)
                           ;; Sometimes, for instance for the invite
                           ;; flow, we want to get a callback before
                           ;; the user is prepared, so we might want
                           ;; to set this to NIL in those cases.
                           (ensure-prepared t))
  (let ((fn (cond
              (ensure-prepared
               (lambda ()
                 (call-with-ensure-user-prepared
                  *installation*
                  (auth:current-user)
                  fn)))
              (t fn))))
   (cond
     ((and
       needs-login
       company
       (company-sso-auth-provider company))
      (call-with-company-login (company-sso-auth-provider company)
                               company
                               fn))
     ((and needs-login (not (auth:current-user)))
      (apply
       (if signup #'signup-get #'signin-get)
       :alert alert
       :redirect
       (cond
         ((and
           allow-url-redirect
           (eql :get (hunchentoot:request-method*)))
          (hunchentoot:request-uri*))
         (t
          (nibble (:name :after-login)
            (funcall fn))))
       (when signup
         (list :invite invite))))
     (t
      (funcall fn)))))


(defun %with-oauth-state-and-redirect (state body)
  (let* ((nibble-id (and state (parse-integer state)))
         (nibble (and nibble-id (nibble:get-nibble nibble-id))))
    (funcall body)
    (cond
      ((null nibble)
       (hex:safe-redirect "/"))
      (t
       (hex:safe-redirect nibble)))))

(hex:def-named-url oauth-callback "/account/oauth-callback")

(hex:def-clos-dispatch ((self auth:auth-acceptor-mixin) "/account/oauth-callback") (code state)
  (declare (ignore code)) ;; will be read from the nibble
  (cond
    ((str:emptyp state)
     (warn "State not present in oauth-callback")
     "Invalid OpenID Connect response, missing state field.")
    (t
     (nibble:render-nibble self state))))

(defmacro with-oauth-state-and-redirect ((state) &body body)
  `(flet ((body () ,@body))
     (%with-oauth-state-and-redirect ,state #'body)))

(defun make-redirect-nibble (redirect)
  (if (stringp redirect)
      (nibble () (hex:safe-redirect redirect))
      redirect))


(defclass ip-throttler (throttler)
  ()
  (:default-initargs :tokens 10))

(defmethod throttle! ((self ip-throttler) &key key &allow-other-keys)
  (call-next-method
   self :key (hunchentoot:real-remote-addr)))

(defgeneric auth-template-impl (installation children &key body-class simple))

(markup:deftag auth-template (children &key body-class simple full-width)
  (auth-template-impl *installation*
                      children :body-class body-class
                      :full-width full-width
                               :simple simple))

(defclass standard-auth-provider (auth-provider
                                  roles-auth-provider)
  ((verify-email-p :initarg :verify-email-p
                   :initform nil
                   :reader verify-email-p)
   (recaptcha-token :initarg :recaptcha-token
                    :initform nil
                    :reader recaptcha-token)))

(defmacro with-login ((&key (needs-login t) (signup nil)
                         (company nil)
                         (invite nil)
                         (ensure-prepared t)
                         (allow-url-redirect nil)
                         (alert nil)) &body body)
  `(server-with-login (lambda () ,@body)
                      :needs-login ,needs-login
                      :invite ,invite
                      :signup ,signup
                      :company ,company
                      :ensure-prepared ,ensure-prepared
                      :allow-url-redirect ,allow-url-redirect
                      :alert ,alert))
