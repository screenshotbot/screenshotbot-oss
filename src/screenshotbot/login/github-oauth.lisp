;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/login/github-oauth
  (:use #:cl
        #:alexandria
        #:screenshotbot/login/common
        #:util/java
        #:screenshotbot/model/user
        #:screenshotbot/user-api
        #:screenshotbot/model/company
        #:screenshotbot/github/access-checks
        #:screenshotbot/model/github)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/installation
                #:installation
                #:auth-providers)
  (:import-from #:screenshotbot/github/jwt-token
                #:github-request)
  (:import-from #:util #:make-url)
  (:import-from #:bknr.datastore
                #:store-objects-with-class
                #:with-transaction)
  (:import-from #:screenshotbot/login/oidc
                #:access-token-str
                #:update-oidc-user
                #:oauth-get-access-token)
  (:import-from #:screenshotbot/events
                #:push-event)
  (:import-from #:nibble
                #:nibble)
  (:export
   #:prepare-gh-user
   #:make-gh-oauth-link
   #:oauth-get-access-token
   #:github-oauth-provider))
(in-package :screenshotbot/login/github-oauth)


(named-readtables:in-readtable java-syntax)


(defclass github-oauth-provider (abstract-oauth-provider)
  ((client-id :initarg :client-id
              :accessor client-id)
   (client-secret :initarg :client-secret
                  :accessor client-secret))
  (:default-initargs
   :oauth-name "GitHub"))

(defun github-oauth-provider ()
  (loop for auth-provider in (auth-providers (installation))
        if (typep auth-provider 'github-oauth-provider)
          return auth-provider))

(defun make-gh-oauth-link (github-oauth redirect
                           &key
                             authorize-link
                             ;; We sometimes need to get access tokens
                             ;; not for login purposes.
                             (access-token-callback #'process-access-token)
                             (scope "user:email"))
  (declare (ignore authorize-link)) ;; too lazy to figure out what this was
  (let ((redirect (nibble:nibble (code error)
                    (cond
                      (code
                       (%gh-oauth-callback
                        github-oauth code
                        redirect
                        :access-token-callback access-token-callback))
                      (t
                       (warn "GitHub Oauth failed: ~a" error)
                       (hex:safe-redirect "/signin" :error error))))))
    (format nil "https://github.com~a"
            (make-url "/login/oauth/authorize"
                      :client_id (client-id github-oauth)
                      :scope scope
                      :state (nibble:nibble-id redirect)
                      :redirect_uri (hex:make-full-url
                                     hunchentoot:*request*
                                     "/account/oauth-callback")))))

(defun handle-github (event auth redirect)
  (nibble ()
    (push-event event)
    (hex:safe-redirect (make-gh-oauth-link auth redirect))))

(defmethod oauth-signin-link ((auth github-oauth-provider) redirect)
  (handle-github :github-oauth.signin auth redirect))

(defmethod oauth-signup-link ((auth github-oauth-provider) redirect)
  (handle-github :github-oauth.signup auth redirect))

(defun process-access-token (access-token)
  (let ((user (get-user-from-gh-access-token access-token)))
    (setf (current-user) user)))

(defun %gh-oauth-callback (auth-provider code redirect &key (access-token-callback #'process-access-token))
  (restart-case
      (progn
        (let ((access-token (oauth-get-access-token
                             "https://github.com/login/oauth/access_token"
                             :client_id (client-id auth-provider)
                             :client_secret (client-secret auth-provider)
                             :code code
                             :redirect_uri (hex:make-full-url hunchentoot:*request*
                                                              'oauth-callback))))
          (funcall access-token-callback access-token)
          (hex:safe-redirect redirect)))
    (retry-gh-oauth-callback ()
      (%gh-oauth-callback auth-provider code redirect))))

(defun get-github-emails (access-token-str)
  (restart-case
      (let ((ret
             (github-request  "/user/emails"
                              :installation-token access-token-str
                              :method :get)))
        ;; Move the Primary email to the top of the list
        (loop for x in ret
              if (assoc-value x :primary)
                do (setf ret
                         (cons x (remove x ret))))
        (loop for x in ret
              if (assoc-value x :verified)
                collect (assoc-value x :email)))
    (retry-get-github-emails ()
      (get-github-emails access-token-str))))

(defun get-user-from-gh-access-token (access-token)
  (let ((access-token (access-token-str access-token)))
    (let* ((client (github-client :oauth-token access-token))
           (user-service (github-user-service client))
           (user (#_getUser user-service))
           (emails  (restart-case
                        (get-github-emails access-token)
                      (use-bad-email ()
                        (list (#_getEmail user))))))
      (prepare-gh-user
       :emails emails
       :user-id (#_getId user)
       :github-login (#_getLogin user)
       :full-name (#_getName user)
       :avatar (#_getAvatarUrl user)))))


(defun prepare-gh-user (&key emails user-id full-name avatar
                                    github-login)
  (assert user-id)
  (let ((gh-user (or
                  (%find-github-user-by-id user-id)
                  (make-instance 'github-user
                                 :gh-user-id user-id))))

    (let ((ret (update-oidc-user gh-user
                                 :email (car emails)
                                 :user-id user-id
                                 :full-name full-name
                                 :avatar avatar)))
      (with-transaction ()
        (setf (github-login gh-user)
              github-login))
      (dolist (email emails)
        (with-transaction ()
          (pushnew email (known-emails gh-user)
                   :test 'equal)))
      ret)))

(defun fix-bad-user-oauth-user-mapping ()
  "Migration. Don't use for anything in particular"
  (loop for x in (store-objects-with-class 'user) do
  (dolist (y (oauth-users x))
    (log:info "In here with ~a, ~a (actually: ~a)" x y (oauth-user-user y))
    (restart-case
        (unless (eql x (oauth-user-user y))
          (error "got mismatch: ~a, ~a (actuall: ~a" x y (oauth-user-user y)))
      (ignore ()
        nil)
      (fix-oauth-user-user ()
        (with-transaction ()
          (setf (oauth-user-user y) x)))))))
