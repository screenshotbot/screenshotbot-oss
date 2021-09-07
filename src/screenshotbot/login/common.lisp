;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/login/common
  (:use #:cl
        #:alexandria
        #:screenshotbot/user-api
        #:screenshotbot/model/user
        #:nibble
        #:screenshotbot/model/company)
  (:import-from #:screenshotbot/server
                #:logged-in-p
                #:defhandler
                #:*nibble-plugin*
                #:server-with-login)
  (:import-from #:screenshotbot/template
                #:landing-head)
  (:import-from #:screenshotbot/installation
                #:auth-provider-signin-form
                #:auth-provider-signup-form
                #:auth-provider)
  (:use-reexport #:screenshotbot/cdn)
  (:export
   #:*current-company-override*
   #:with-oauth-state-and-redirect
   #:make-redirect-nibble
   #:signin-get
   #:auth-template
   #:oauth-name
   #:oauth-callback
   #:oauth-logo-svg
   #:oauth-signin-link
   #:oauth-signup-link
   #:abstract-oauth-provider))
(in-package :screenshotbot/login/common)

(markup:enable-reader)

(defclass user-view ()
  ())

(defvar *current-company-override* nil)

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

(defmethod auth-provider-signup-form ((auth-provider abstract-oauth-provider)
                                      invite-code
                                      plan
                                      redirect)
    <div class= "form-group mt-3 text-center mb-3">
      <a class= "btn btn-secondary" style= "width:100%"  href= (oauth-signup-link auth-provider redirect) >
        ,(oauth-logo-svg auth-provider)
        <span class= "ms-1">Sign Up with ,(oauth-name auth-provider) </span>
      </a>
    </div>)

(defmethod auth:user-class ((user-view user-view))
  'user)

(defun (setf current-user) (user)
  (setf (auth:session-value :user) user)
  user)

(defun needs-user! ()
  (unless (logged-in-p)
    (hex:safe-redirect "/")))


(defun server-with-login (fn &key needs-login)
  (cond
    ((and needs-login (not (current-user)))
     (signin-get :redirect (nibble ()
                             (funcall fn))))
    (t
     (funcall fn))))


(defun current-user ()
  (when (logged-in-p)
    (cond
      ((and (boundp '*current-api-key*)
            *current-api-key*)
       (api-key-user *current-api-key*))
      (t
       (let ((user (auth:session-value :user)))
         (check-type user user)
         user)))))

(defun (Setf current-company) (company)
  (can-view! company)
  (setf (auth:session-value :company) company))

(defun current-company (&key (user nil user-bound-p))
  (cond
    (*current-company-override*
     *current-company-override*)
    (user-bound-p
     (let* ((company (auth:session-value :company)))
       (if (and company (can-view company user))
           company
           (or
            (user-personal-company user)
            (car (user-companies user))))))
    ((not (logged-in-p))
     nil)
    ((boundp '*current-api-key*)
     (api-key-company *current-api-key*))
    (t
     (current-company :user (current-user)))))

(defun logged-in-p ()
  (or
   (when (auth:session-value :user) t)
   (boundp '*current-api-key*)))


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
   *nibble-plugin*
   state))

(defmacro with-oauth-state-and-redirect ((state) &body body)
  `(flet ((body () ,@body))
     (%with-oauth-state-and-redirect ,state #'body)))

(defun make-redirect-nibble (redirect)
  (if (stringp redirect)
      (nibble () (hex:safe-redirect redirect))
      redirect))

(markup:deftag auth-template (children)
  <html lang= "en" >
    <landing-head  />
    <body >
      ,@ (progn children)
    </body>
  </html>)
