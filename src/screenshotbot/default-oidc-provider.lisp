;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/default-oidc-provider
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class)
  (:import-from #:screenshotbot/login/oidc
                #:find-existing-oidc-user
                #:oauth-user-user
                #:oidc-provider)
  (:import-from #:oidc/oidc
                #:authentication-error
                #:authentication-error-message
                #:oidc-callback
                #:after-authentication
                #:make-oidc-auth-link)
  (:import-from #:util/store/object-id
                #:object-with-oid)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:screenshotbot/installation
                #:site-admins-can-use-sso-p
                #:multi-org-feature)
  (:import-from #:screenshotbot/model/user
                #:adminp)
  (:import-from #:screenshotbot/model/company
                #:get-singleton-company)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:util/store/fset-index
                #:fset-set-index
                #:fset-unique-index)
  (:export
   #:default-oidc-provider
   #:oidc-providers-for-company
   #:issuer
   #:oidc-provider-company
   #:client-id
   #:oidc-edited-at
   #:edit-default-oidc-provider
   #:make-oidc-provider
   #:active-default-oidc-provider
   #:activep
   #:activate-default-oidc-provider
   #:deactivate-default-oidc-provider
   #:delete-default-oidc-provider
   #:oidc-activation-blocker
   #:oidc-test-signin-link
   #:last-oidc-test-result
   #:oidc-test-result-email
   #:oidc-test-result-error
   #:oidc-test-result-ts
   #:oidc-test-result-tested-by))
(in-package :screenshotbot/default-oidc-provider)

(defvar *lock* (bt:make-lock))

(defindex +active-index+
  'fset-unique-index
  :slot-name '%activep)

(defindex +company-index+
  'fset-set-index
  :slot-name '%company)

(defclass default-oidc-provider (object-with-oid)
  ((%scope :initarg :scope
          :reader scope)
   (%client-id :initarg :client-id
               :reader client-id
               :writer (setf %client-id))
   (%client-secret :initarg :client-secret
                   :reader client-secret
                   :writer (setf %client-secret))
   (%expiration :initarg :expiration-seconds
                :initform (* 20 3600)
                :reader expiration)
   (%issuer :initarg :issuer
            :reader issuer
            :writer (setf %issuer))
   (%company :initarg :company
             :index +company-index+
             :index-reader oidc-providers-for-company
             :reader company
             :reader oidc-provider-company)
   (%activep :initarg :activep
             :initform nil
             :index +active-index+
             :index-reader %find-by-activep
             :reader activep
             :writer (setf %activep)
             :documentation "Whether this is the login for the installation. At most one
provider can be active, see ACTIVATE-DEFAULT-OIDC-PROVIDER.")
   (%edited-at :initarg :edited-at
               :initform nil
               :reader oidc-edited-at
               :writer (setf %edited-at)
               :documentation "When the IdP config last changed. Tests from before this
don't count."))
  (:metaclass persistent-class))

(defun active-default-oidc-provider ()
  (%find-by-activep t))

(defun activate-default-oidc-provider (provider user)
  "Make PROVIDER the login for this installation, replacing the currently
active provider if there is one. USER is who's activating it, see
OIDC-ACTIVATION-BLOCKER."
  (bt:with-lock-held (*lock*)
    (let ((blocker (oidc-activation-blocker provider user)))
      (when blocker
        (error "Can't activate OIDC provider: ~a" blocker)))
    (%activate-default-oidc-provider provider)))

(defun %activate-default-oidc-provider (provider)
  "Like ACTIVATE-DEFAULT-OIDC-PROVIDER, but without any checks. Call it
with *LOCK* held."
  (let ((old (active-default-oidc-provider)))
    ;; Clear the old one first, %ACTIVEP has a unique index.
    (when old
      (setf (%activep old) nil))
    (setf (%activep provider) t)))

(defun edit-default-oidc-provider (provider &key issuer client-id client-secret)
  "Change PROVIDER's IdP config. A NIL CLIENT-SECRET keeps the current
secret. Changing the active provider isn't allowed: it would change
everyone's login without a test. Create a new provider and activate it
instead."
  (bt:with-lock-held (*lock*)
    (when (bknr.indices:object-destroyed-p provider)
      (error "This OIDC connection was deleted"))
    (let* ((client-secret (or client-secret (client-secret provider)))
           (changedp (not (and (equal issuer (issuer provider))
                               (equal client-id (client-id provider))
                               (equal client-secret (client-secret provider))))))
      (when changedp
        (when (activep provider)
          (error "Can't change the active OIDC provider"))
        (setf (%issuer provider) issuer)
        (setf (%client-id provider) client-id)
        (setf (%client-secret provider) client-secret)
        (setf (%edited-at provider) (get-universal-time))))))

(defun deactivate-default-oidc-provider (provider)
  (setf (%activep provider) nil))

(defun delete-default-oidc-provider (provider)
  "Delete PROVIDER and its test results. The active provider can't be
deleted, since that would lock everyone out: activate another provider
first."
  (bt:with-lock-held (*lock*)
    (when (activep provider)
      (error "Can't delete the active OIDC provider"))
    (fset:do-set (result (oidc-test-results-for-provider provider))
      (bknr.datastore:delete-object result))
    (bknr.datastore:delete-object provider)))

(defclass stored-oidc-provider (oidc-provider)
  ()
  (:documentation "The OIDC-PROVIDER for a DEFAULT-OIDC-PROVIDER. Unlike the one from the
installation config, a company admin configured it, so we don't trust
it with site admins, see SITE-ADMINS-CAN-USE-SSO-P."))

(defmethod after-authentication :before ((auth stored-oidc-provider) &key user-id email
                                         &allow-other-keys)
  ;; The user we'd sign in as, see PREPARE-OIDC-USER. Check before
  ;; that, so we don't link this IdP to the site admin's user.
  (let ((user (or
               (let ((oidc-user (find-existing-oidc-user auth user-id)))
                 (when oidc-user
                   (oauth-user-user oidc-user)))
               (auth:find-user *installation* :email email))))
    (when (and user
               (adminp user)
               (not (site-admins-can-use-sso-p *installation*)))
      (error 'authentication-error
             :message "Site admins can't sign in with this identity provider"))))

(defun make-oidc-provider (stored-provider &rest initargs &key (class 'stored-oidc-provider)
                                                              &allow-other-keys)
  "Create an oidc-provider from a default-oidc-provider instance. CLASS and
the other INITARGS are passed to MAKE-INSTANCE."
  (apply #'make-instance class
                 :issuer (issuer stored-provider)
                 :client-id (client-id stored-provider)
                 :client-secret (client-secret stored-provider)
                 :scope (scope stored-provider)
                 :identifier 'default-oidc-provider
                 :company-provider (lambda (&rest args)
                                     (declare (ignore args))
                                     (company stored-provider))
                 :expiration-seconds (expiration stored-provider)
                 (alexandria:remove-from-plist initargs :class)))

(defindex +test-result-provider-index+
  'fset-set-index
  :slot-name '%provider)

(defclass oidc-test-result (store-object)
  ((%provider :initarg :provider
              :index +test-result-provider-index+
              :index-reader oidc-test-results-for-provider)
   (tested-by :initarg :tested-by
              :reader oidc-test-result-tested-by)
   (email :initarg :email
          :initform nil
          :reader oidc-test-result-email
          :documentation "The email from a successful test")
   (error-message :initarg :error-message
                  :initform nil
                  :reader oidc-test-result-error
                  :documentation "Why the login failed, NIL on success")
   (ts :initarg :ts
       :initform (get-universal-time)
       :reader oidc-test-result-ts))
  (:metaclass persistent-class)
  (:documentation "The outcome of a test login against a DEFAULT-OIDC-PROVIDER, used to
make sure a provider works before it's activated."))

(defun last-oidc-test-result (provider)
  ;; Not by TS: that's in seconds, so two tests in the same second
  ;; would be ambiguous. Object IDs increase with creation order.
  (let ((results (fset:convert 'list (oidc-test-results-for-provider provider))))
    (first (sort results #'> :key #'bknr.datastore:store-object-id))))

(defclass test-oidc-provider (stored-oidc-provider)
  ((stored-provider :initarg :stored-provider
                    :reader stored-provider)
   (tested-by :initarg :tested-by
              :reader tested-by))
  (:documentation "Like the OIDC-PROVIDER for STORED-PROVIDER, but a login only records
an OIDC-TEST-RESULT, it doesn't log anyone in."))

(defun record-test-result (auth &key email error-message)
  (when (bknr.indices:object-destroyed-p (stored-provider auth))
    ;; The provider was deleted after this test started.
    (error "This OIDC connection was deleted"))
  (make-instance 'oidc-test-result
                 :provider (stored-provider auth)
                 :tested-by (tested-by auth)
                 :email email
                 :error-message error-message))

(defmethod oidc-callback :around ((auth test-oidc-provider) code redirect
                                  &key error error-description &allow-other-keys)
  "Record failures as a test result instead of showing an error page.
On success, AFTER-AUTHENTICATION records the result and the :AFTER
method redirects to REDIRECT."
  (flet ((record-error (message)
           (record-test-result auth :error-message message)
           (hex:safe-redirect redirect)))
    (cond
      ((not code)
       (record-error (format nil "~a: ~a"
                             error
                             (or error-description "No details provided"))))
      (t
       (handler-case
           (call-next-method)
         (error (e)
           (record-error (princ-to-string e))))))))

(defmethod after-authentication :around ((auth test-oidc-provider) &key &allow-other-keys)
  ;; OIDC-CALLBACK would show an error page for these, record them as
  ;; a failed test instead.
  (handler-case
      (call-next-method)
    (authentication-error (e)
      (record-test-result auth :error-message (authentication-error-message e)))))

(defmethod after-authentication ((auth test-oidc-provider) &key email &allow-other-keys)
  (cond
    ((str:non-blank-string-p email)
     (record-test-result auth :email email))
    (t
     (record-test-result auth :error-message "The IdP didn't return an email address"))))

(defun oidc-test-signin-link (provider tested-by redirect)
  "A link to test PROVIDER as TESTED-BY. After the login, the result is
recorded and the browser is sent to REDIRECT."
  (check-type tested-by store-object)
  (make-oidc-auth-link
   (make-oidc-provider provider
                       :class 'test-oidc-provider
                       :stored-provider provider
                       :tested-by tested-by)
   redirect))

(defparameter *activation-test-window* (* 60 60)
  "How recent the test must be when activating a provider.")

(defun oidc-activation-blocker (provider user)
  "NIL if USER may make PROVIDER the login for this installation,
otherwise a message explaining why not.

Activating replaces the login for everyone, so we require a recent
successful test by USER. We don't require the test to sign in as an
admin: if nobody can administer the installation after activating, an
admin can be restored with a recovery token."
  (let ((result (last-oidc-test-result provider)))
    (cond
      ((typep *installation* 'multi-org-feature)
       ;; There's only one active provider for the whole installation,
       ;; it would make this company's IdP everyone's login.
       "Setting a default OIDC connection is only supported on single-company installations")
      ((not (eql (company provider)
                 (get-singleton-company *installation*)))
       "This provider doesn't belong to this installation's company")
      ((activep provider)
       "This provider is already the default")
      ((null result)
       "Test this provider before setting it as the default")
      ((oidc-test-result-error result)
       "The last test failed")
      ((not (eql user (oidc-test-result-tested-by result)))
       "Test this provider yourself before setting it as the default")
      ((< (oidc-test-result-ts result)
          (- (get-universal-time) *activation-test-window*))
       "The last test is more than an hour old, test it again")
      ((and (oidc-edited-at provider)
            ;; <=: in the same second we can't tell which came first
            (<= (oidc-test-result-ts result)
                (oidc-edited-at provider)))
       "The provider changed since the last test, test it again")
      (t
       nil))))
