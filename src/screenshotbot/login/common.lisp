;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/common
  (:use :cl)
  (:import-from #:core/installation/auth
                #:call-with-ensure-user-prepared
                #:company-for-request)
  (:import-from #:core/installation/auth-provider
                #:auth-provider
                #:auth-provider-signin-form
                #:auth-provider-signup-form)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:nibble
                #:nibble
                #:nibble-id)
  (:import-from #:screenshotbot/model/company
                #:company
                #:sso-auth-provider)
  (:import-from #:screenshotbot/server
                #:defhandler
                #:logged-in-p
                #:server-with-login)
  (:import-from #:screenshotbot/sso/model
                #:call-with-company-login)
  (:import-from #:screenshotbot/user-api
                #:can-view!
                #:current-company
                #:signup-get
                #:user)
  (:import-from #:util/throttler
                #:throttle!
                #:throttler)
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
   #:with-oauth-state-and-redirect))
(in-package :screenshotbot/login/common)

(named-readtables:in-readtable markup:syntax)

(defclass abstract-oauth-provider (auth-provider)
  ((oauth-name :initarg :oauth-name
               :accessor oauth-name)))

(defmethod auth-provider-signin-form ((auth-provider abstract-oauth-provider) redirect)
    <div class= "form-group mt-3 text-center mb-3">
      <a class= "btn btn-secondary" style= "width:100%"  href= (oauth-signin-link auth-provider redirect) >
        ,(oauth-logo-svg auth-provider)
        <span class= "ms-1">Sign In with ,(oauth-name auth-provider) </span>
      </a>
    </div>)

(defgeneric after-create-user (installation user)
  (:method (installation user)
    (declare (ignore installation user))
    (values)))

(defmethod auth-provider-signup-form ((auth-provider abstract-oauth-provider)
                                      invite-code
                                      plan
                                      redirect)
    <div class= "form-group mt-3 text-center mb-3">
      <a class= "btn btn-secondary" style= "width:100%"  href= (oauth-signup-link auth-provider redirect) >
        ,(oauth-logo-svg auth-provider)
        <span class= "ms-1">,(oauth-name auth-provider) </span>
      </a>
    </div>)

(defun server-with-login (fn &key needs-login signup alert company
                           ;; Redirect a GET request back to the
                           ;; original URL instead of a nibble.
                           (allow-url-redirect nil)
                           ;; Sometimes, for instance for the invite
                           ;; flow, we want to get a callback before
                           ;; the user is prepared.
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
       (sso-auth-provider company))
      (call-with-company-login (sso-auth-provider company)
                               company
                               fn))
     ((and needs-login (not (auth:current-user)))
      (funcall
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
            (funcall fn))))))
     (t
      (funcall fn)))))


(defmethod auth:authenticate-request ((request screenshotbot/server:request))
  (unless (auth:request-user request) ;; Might happen in tests
    (alexandria:when-let ((user (auth:session-value :user)))
      (check-type user user)
      (setf (auth:request-user request) user)))
  (unless (auth:request-account request)
    (alexandria:when-let ((company (company-for-request *installation* request)))
      (setf (auth:request-account request) company))))


(defun (setf current-company) (company)
  (can-view! company)
  (setf (auth:session-value :company) company
        (auth:request-account hunchentoot:*request*) company))

(defun current-company ()
  (and
      (boundp 'hunchentoot:*request*)
      (auth:request-account hunchentoot:*request*)))

(defun logged-in-p ()
  (auth:current-user))


(defun %with-oauth-state-and-redirect (state body)
  (let* ((nibble-id (and state (parse-integer state)))
         (nibble (and nibble-id (nibble:get-nibble nibble-id))))
    (funcall body)
    (cond
      ((null nibble)
       (hex:safe-redirect "/"))
      (t
       (hex:safe-redirect nibble)))))

(defhandler (oauth-callback :uri "/account/oauth-callback") (code state)
  (declare (ignore code)) ;; will be read from the nibble
  (assert state)
  (nibble:render-nibble
   hunchentoot:*acceptor*
   state))

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
