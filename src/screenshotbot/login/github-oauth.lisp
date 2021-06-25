;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/login/github-oauth
    (:use #:cl
          #:alexandria
          #:./common
          #:../java
          #:../user-api
          #:../model/user
          #:../model/company
          #:../github/access-checks
          #:../model/github)
  (:import-from #:../server
                #:defhandler)
  (:import-from #:../installation
                #:installation
                #:auth-providers)
  (:import-from #:../github/jwt-token
                #:github-request)
  (:import-from #:util
                #:make-url)
  (:import-from #:bknr.datastore
                #:store-objects-with-class
                #:with-transaction)
  (:export #:prepare-gh-user
           #:prepare-oauth-user
           #:make-gh-oauth-link
           #:oauth-get-access-token
           #:github-oauth-provider))

(named-readtables:in-readtable java-syntax)


(defclass github-oauth-provider (abstract-oauth-provider)
  ((oauth-name :initform "GitHub")
   (client-id :initarg :client-id
              :accessor client-id)
   (client-secret :initarg :client-secret
                  :accessor client-secret)))

(defun make-gh-oauth-link (github-oauth redirect &key authorize-link)
  (declare (ignore authorize-link)) ;; too lazy to figure out what this was
  (let ((redirect (nibble:nibble (code)
                    (%gh-oauth-callback
                     github-oauth code
                     redirect))))
   (format nil "https://github.com~a"
           (make-url "/login/oauth/authorize"
                     :client_id (client-id github-oauth)
                     :scope "user:email"
                     :state (nibble:nibble-id redirect)
                     :redirect_uri (hex:make-full-url
                                    hunchentoot:*request*
                                    "/account/oauth-callback")))))

(defmethod oauth-signin-link ((auth github-oauth-provider) redirect)
  (declare (ignore auth))
  (make-gh-oauth-link auth redirect))

(defmethod oauth-signup-link ((auth github-oauth-provider) redirect)
  (declare (ignore auth))
  (make-gh-oauth-link auth redirect))


(defhandler (nil :uri "/gh-oauth-callback") (code state)
  (error "DEPRECATED callback"))

(defun %gh-oauth-callback (auth-provider code redirect)
  (restart-case
      (progn
        (let ((access-token (oauth-get-access-token
                             "https://github.com/login/oauth/access_token"
                             'github-access-token
                             :client_id (client-id auth-provider)
                             :client_secret (client-secret auth-provider)
                             :code code
                             :redirect_uri (hex:make-full-url hunchentoot:*request*
                                                              'oauth-callback))))
          (let ((user (get-user-from-gh-access-token access-token)))
            (with-transaction ()
              (setf (access-token (github-user user))
                    access-token))
            (setf (current-user) user))
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
  (with-slots (access-token) access-token
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


(defun prepare-oauth-user (oauth-user &key
                                        (email (error "required"))
                                        (user-id (error "required"))
                                        (full-name (error "required"))
                                        (avatar (error "required")))
  (declare (ignore user-id))
  (with-transaction (:initial-setup)
    (setf (oauth-user-email oauth-user) email)
    (setf (oauth-user-full-name oauth-user) full-name)
    (setf (oauth-user-avatar oauth-user) avatar))
  (let ((user (or
               (oauth-user-user oauth-user)
               (user-with-email email)
               (make-instance 'user
                              :email email))))
    (with-transaction (:ensure-two-way-mapping)
      ;; ensure two way mapping.
      (pushnew oauth-user (oauth-users user))
      (setf (oauth-user-user oauth-user) user))
    ;; what happens if there's another user with the current email?
    ;; For instance, if the Oauth session changed their email address?
    ;; In that case, we ignore and don't make the email change.
    (ignore-errors
     (with-transaction (:set-user-email)
       (setf (user-email user) email)))
    user))

(defun prepare-gh-user (&key emails user-id full-name avatar
                                    github-login)
  (assert user-id)
  (let ((gh-user (or
                  (%find-github-user-by-id user-id)
                  (make-instance 'github-user
                                 :gh-user-id user-id))))

    (let ((ret (prepare-oauth-user gh-user
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
;;(with-db (get-user-from-gh-access-token (find-by-id 'github-access-token 4)))
