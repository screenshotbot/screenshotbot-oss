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
                #:call-with-ensure-user-prepared
                #:installation
                #:*installation*
                #:multi-org-feature
                #:auth-provider-signin-form
                #:auth-provider-signup-form
                #:auth-provider)
  (:use-reexport #:screenshotbot/cdn)
  (:import-from #:screenshotbot/user-api
                #:signup-get)
  (:import-from #:local-time
                #:timestamp-
                #:timestamp>)
  (:import-from #:screenshotbot/model/company
                #:sso-auth-provider
                #:get-singleton-company)
  (:import-from #:screenshotbot/sso/model
                #:call-with-company-login)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/model/recorder-run
                #:runs-for-company)
  (:import-from #:util/throttler
                #:throttle!
                #:throttler)
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
   #:abstract-oauth-provider
   #:after-create-user))
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

(defmethod auth:user-class ((user-view user-view))
  'user)

(defun (setf current-user) (user &key expires-in)
  (setf (auth:session-value :user :expires-in expires-in) user)
  (setf (auth:request-user hunchentoot:*request*) user)
  user)

(defun needs-user! ()
  (unless (logged-in-p)
    (hex:safe-redirect "/")))


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
                  (installation)
                  (current-user)
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
     ((and needs-login (not (current-user)))
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


(defun current-user ()
  (and
   (boundp 'hunchentoot:*request*)
   (auth:request-user hunchentoot:*request*)))

(defmethod auth:authenticate-request ((request screenshotbot/server:request))
  (unless (auth:request-user request) ;; Might happen in tests
    (alexandria:when-let ((user (auth:session-value :user)))
      (check-type user user)
      (setf (auth:request-user request) user)))
  (unless (auth:request-account request)
    (alexandria:when-let ((company
                           (typecase (installation)
                             (multi-org-feature
                              (cond
                                ((not (logged-in-p))
                                 nil)
                                (t
                                 (guess-best-company
                                  (auth:session-value :company)
                                  (auth:request-user request)))))
                             (t
                              (get-singleton-company (installation))))))
      (setf (auth:request-account request) company))))

(defun guess-best-company (company user)
  (when user
   (if (and company (can-view company user))
       company
       (or
        (most-recent-company (user-companies user))
        (user-personal-company user)
        (car (user-companies user))))))

(defmethod nibble:nibble-current-user ((acceptor screenshotbot/server:acceptor))
  (current-user))

(defun (setf current-company) (company)
  (can-view! company)
  (setf (auth:session-value :company) company
        (auth:request-account hunchentoot:*request*) company))

(defun most-recent-company (companies)
  "Returns the most recently updated company in the list. If none are
  updated in the last month, then return the personal company"
  (cdar
   (sort
    (let ((cutoff (timestamp- (local-time:now) 60 :day)))
      (loop for company in companies
            for run = (fset:greatest (runs-for-company company))
            for created-at = (when run (created-at run))
            if (and created-at (timestamp> created-at cutoff))
              collect
              (cons created-at company)))
    #'timestamp>
      :key #'car)))

(defun current-company ()
  (and
      (boundp 'hunchentoot:*request*)
      (auth:request-account hunchentoot:*request*)))

(defun logged-in-p ()
  (current-user))


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

(markup:deftag auth-template (children &key body-class simple)
  <html lang= "en" >
    <landing-head>
      ,(progn
         ;; this is required in the OSS version because of it's using
         ;; a hacky mix of the pro and OSS dashboard css.
         #+screenshotbot-oss
         <style>
           html {
           font-size: 10px;
           }
         </style>)

    </landing-head>

    ,(cond
       (simple
        <body class= "">
          ,@ (progn children)
        </body>)
       (t
        <body class= (format nil "auth-pages ~a" body-class) >


          <div class= "left-image">
            <a class= "navbar-brand" href= "/">
              <img src= "/assets/images/logo-dark.png" />
            </a>
            <img class= "botty-image"  src= "/assets/images/auth/botty-left.png" />

            <span class= "copy" >
              &copy; 2018-2024 Modern Interpreters Inc.
            </span>
          </div>

          <div class= "form-container">
            <div class= "home-link">
              <a href= "/">Home</a>
            </div>
            ,@ (progn children)
          </div>
        </body>))

  </html>)

(defclass ip-throttler (throttler)
  ()
  (:default-initargs :tokens 10))

(defmethod throttle! ((self ip-throttler) &key key &allow-other-keys)
  (call-next-method
   self :key (hunchentoot:real-remote-addr)))
