;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/default-oidc-provider
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:with-transaction
                #:store-object
                #:persistent-class)
  (:import-from #:screenshotbot/login/oidc
                #:oidc-provider)
  (:import-from #:oidc/oidc
                #:oidc-callback
                #:after-authentication
                #:make-oidc-auth-link)
  (:import-from #:util/store/object-id
                #:object-with-oid)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-set-index
                #:fset-unique-index)
  (:export
   #:default-oidc-provider
   #:oidc-providers-for-company
   #:issuer
   #:oidc-provider-company
   #:make-oidc-provider
   #:active-default-oidc-provider
   #:activep
   #:activate-default-oidc-provider
   #:deactivate-default-oidc-provider
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
               :reader client-id)
   (%client-secret :initarg :client-secret
                   :reader client-secret)
   (%expiration :initarg :expiration-seconds
                :initform (* 20 3600)
                :reader expiration)
   (%issuer :initarg :issuer
            :reader issuer)
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
provider can be active, see ACTIVATE-DEFAULT-OIDC-PROVIDER."))
  (:metaclass persistent-class))

(defun active-default-oidc-provider ()
  (%find-by-activep t))

(defun activate-default-oidc-provider (provider)
  "Make PROVIDER the login for this installation, replacing the currently
active provider if there is one."
  (bt:with-lock-held (*lock*)
    (let ((old (active-default-oidc-provider)))
      (with-transaction ()
        ;; Clear the old one first, %ACTIVEP has a unique index.
        (when old
          (setf (%activep old) nil))
        (setf (%activep provider) t)))))

(defun deactivate-default-oidc-provider (provider)
  (with-transaction ()
    (setf (%activep provider) nil)))

(defun make-oidc-provider (stored-provider &rest initargs &key (class 'oidc-provider)
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

(defclass test-oidc-provider (oidc-provider)
  ((stored-provider :initarg :stored-provider
                    :reader stored-provider)
   (tested-by :initarg :tested-by
              :reader tested-by))
  (:documentation "Like the OIDC-PROVIDER for STORED-PROVIDER, but a login only records
an OIDC-TEST-RESULT, it doesn't log anyone in."))

(defun record-test-result (auth &key email error-message)
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
