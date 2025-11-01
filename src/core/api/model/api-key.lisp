;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/api/model/api-key
  (:use :cl)
  (:nicknames :screenshotbot/model/api-key)
  (:import-from #:bknr.datastore
                #:deftransaction
                #:persistent-class
                #:store-object
                #:store-object-id
                #:with-transaction)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:util/misc
                #:make-mp-hash-table)
  (:import-from #:util/store/fset-index
                #:fset-unique-index
                #:fset-set-index
                #:index-least)
  (:import-from #:util/store/store
                #:defindex
                #:with-class-validation)
  (:import-from #:screenshotbot/api-key-api
                #:api-key-key
                #:api-key
                #:api-key-secret-key
                #:delete-api-key)
  (:import-from #:util/store/store-migrations
                #:def-store-migration)
  (:import-from #:core/installation/installation
                #:*installation*
                #:installation-domain)
  (:import-from #:util/atomics
                #:atomic-exchange)
  (:export
   #:%find-api-key
   #:api-key
   #:api-key-company
   #:api-key-description
   #:api-key-key
   #:api-key-secret-key
   #:api-key-user
   #:delete-api-key
   #:make-transient-key
   #:render-api-token
   #:company-api-keys
   #:api-key-for-secret
   #:validate-api-key-secret
   #:make-encoded-secret))
(in-package :core/api/model/api-key)

(defvar *lock* (bt:make-lock "random-string"))

(defvar *generator* (make-instance 'secure-random::open-ssl-generator))

(defparameter +secret-length+ 40)

(defun %all-alpha (&optional (from #\a) (to #\z))
  (let ((From (char-code from))
        (to (char-code to)))
    (assert (< from to))
   (loop for i from from to to collect
        (code-char i))))

(defun %random (num)
  (secure-random:number num *generator*))

(defun generate-random-string (length chars)
  (bt:with-lock-held (*lock*)
    (coerce (loop repeat length collect (aref chars (%random (length chars))))
            'string)))


(defun generate-api-key ()
  (generate-random-string 20 (concatenate 'string (%all-alpha #\A #\Z)
                                          (%all-alpha #\0 #\9))))

(Defun generate-api-secret ()
  (generate-random-string +secret-length+ (concatenate 'string
                                                       (%all-alpha #\A #\Z)
                                                       (%all-alpha #\a #\z)
                                                       (%all-alpha #\0 #\9))))

(defindex +expires-index+
  'fset-set-index
  :slot-name 'expires-at)

(defindex +key-index+
  'fset-unique-index
  :slot-name 'api-key)

(defindex +secret-index+
  'fset-unique-index
  :slot-name 'api-secret-key)

(with-class-validation
  (defclass api-key (store-object)
    ((user
      :initarg :user
      :accessor api-key-user)
     (company
      :initarg :company
      :initform nil
      :accessor api-key-company)
     (api-key
      :type string
      :accessor api-key-key
      :initarg :api-key
      :index +key-index+
      :index-reader %%find-api-key
      :initform nil)
     (api-secret-key
      :type string
      :initarg :api-secret-key
      :accessor api-key-secret-key
      :index +secret-index+
      :index-reader api-key-for-secret
      :initform nil)
     (%description
      :type string
      :initarg :description
      :initform nil
      :accessor api-key-description)
     (expired-p
      :type boolean
      :reader expired-p
      :initform nil)
     (%permissions
      :initarg :permissions
      :accessor api-key-permissions)
     (last-used
      :initarg :last-used
      :accessor last-used))
    (:metaclass persistent-class)
    (:default-initargs
     :api-key (generate-api-key)
     :last-used (get-universal-time)
     :api-secret-key (generate-api-secret)
     :permissions nil)))

(defmethod last-used :around ((self api-key))
  (ignore-errors
   (call-next-method)))

(defmethod api-hostname (installation)
  (installation-domain installation))

(defun make-encoded-secret (api-key-string hostname secret)
  "Create an encoded secret from the api-key string, hostname, and secret.
Returns the full encoded secret string."
  (flet ((make-encoded (&key (padding ""))
           (str:join
           ","
           (list
            api-key-string
            (format nil "~a1" padding)
            hostname
            ;; Add a trailing comma to make it easier to decode if
            ;; +secret-length+ changes
            ""))))
   (let ((result-pre (make-encoded
                      :padding
                      (str:join
                       ""
                       (loop for i below (- 3 (mod (length (make-encoded)) 3))
                             collect " ")))))
     (let ((result (base64:string-to-base64-string
                    result-pre)))
       (assert (not (str:containsp "+" result)))
       (assert (not (str:containsp "/" result)))
       (assert (not (str:containsp "=" result)))
       (format nil "~a~a" result secret)))))

(defmethod encode-api-token ((self api-key))
  (make-encoded-secret
   (api-key-key self)
   (api-hostname *installation*)
   (api-key-secret-key self)))


(defmethod expires-at ((self api-key))
  nil)

(with-class-validation
  (defclass cli-api-key (api-key)
    ((expires-at
      :initarg :expires-at
      :index +expires-index+
      :accessor expires-at))
    (:metaclass persistent-class)
    (:default-initargs
     :api-key nil ;; We're going to use cli-NNN instead
     :expires-at (+ 3600 (get-universal-time)))))

(defmethod api-key-key ((self cli-api-key))
  (format nil "cli-~a"
          (store-object-id self)))

(defmethod expired-p ((self cli-api-key))
  (and
   (expires-at self)
   (< (expires-at self) (get-universal-time))))

(defvar *transient-keys* (make-mp-hash-table :test #'equal))

(defclass transient-api-key ()
  ((api-key
    :initform (format nil "tmp_~a" (generate-api-key))
    :reader api-key-key)
   (api-key-secret
    :initform (generate-api-secret)
    :reader api-key-secret-key)
   (user
    :initarg :user
    :reader api-key-user)
   (company
    :initarg :company
    :reader api-key-company)
   (%last-used
    :accessor last-used
    :documentation "This is just to be in sync with api-key for simplicity.")
   (created-at :initform (get-universal-time)))
  (:documentation "A transient key generated for communication between
  the replay service and this server."))

(defmethod expired-p ((self transient-api-key))
  nil)

(defvar *last-used-cache* (fset:empty-map))

(defun mark-api-key-used (api-key)
  (when api-key
   (let ((ts (get-universal-time)))
     (atomics:atomic-update
      *last-used-cache*
      (lambda (map)
        (fset:with map api-key ts))))))

(defun flush-last-used-cache ()
  (let ((last-used-cache (atomic-exchange *last-used-cache* (fset:empty-map))))
    (fset:do-map (api-key ts last-used-cache)
      (setf (last-used api-key) ts))))

(def-cron flush-last-used-cache ()
  (flush-last-used-cache))

(defun %find-api-key (str)
  "TODO(T1681): Rename to find-api-key (without prefix)"
  (let ((result (or
                 (cond
                   ((str:starts-with-p "cli-" str)
                    (let ((res (bknr.datastore:store-object-with-id
                                (parse-integer (second (str:split "-" str :limit 2))))))
                      (when (typep res 'cli-api-key)
                        res)))
                   (t
                    (%%find-api-key str)))
                 (gethash str *transient-keys*))))
    (when (and
           result
           (not (expired-p result)))
      result)))

(defun make-transient-key (&key user company)
  (let ((key (make-instance 'transient-api-key
                             :user user
                             :company company)))
    (setf (gethash (api-key-key key) *transient-keys*)
          key)
    key))

(defun decode-api-token (token)
  (let ((encoded (str:substring 0 (- (length token) +secret-length+) token))
        (secret (str:substring (- (length token) +secret-length+) nil token)))
   (destructuring-bind (key version url empty-string)
       (str:split "," (base64:base64-string-to-string encoded))
     (assert (equal  "" empty-string))
     (values key secret url))))


(defun validate-api-key-secret (api-key provided-secret)
  "Validates that the provided secret matches the api-key's secret.
Returns T if valid, NIL otherwise."
  (or
   (equal provided-secret (api-key-secret-key api-key))
   (ignore-errors
    (multiple-value-bind (key secret) (decode-api-token provided-secret)
      (declare (ignore key))
      (equal secret (api-key-secret-key api-key))))))


(defmethod render-api-token ((self api-key))
  (format nil "cli-~a:~a"
          (store-object-id self)
          (api-key-secret-key self)))

(deftransaction tx-delete-api-key (api-key)
  "History: we've been using slot-makunbound to delete api
keys. However, slot-makunbound doesn't create a transaction
currently (we need to fix this).

In the meantime, we're using a transaction to fix this. We probably
need a better deletion model in the future."
  (slot-makunbound api-key 'api-key))


(defmethod delete-api-key ((api-key api-key))
  (tx-delete-api-key api-key))

(defmethod company-api-keys (company)
  (loop for api-key in (bknr.datastore:class-instances 'api-key)
        if (and (slot-boundp api-key 'api-key) ;; See T929
                (eq company (api-key-company api-key)))
          collect api-key))

(defmethod user-api-keys (user company)
  (loop for api-key in (company-api-keys company)
        if (eq user (api-key-user api-key))
          collect api-key))

(defmethod cleanup-expired-api-keys ()
  (let ((next (index-least +expires-index+))
        (now (get-universal-time)))
    (when (and next (< (expires-at next) now))
      (bknr.datastore:delete-object next)
      (cleanup-expired-api-keys))))

(def-cron cleanup-expired-api-keys (:step-min 5)
  (cleanup-expired-api-keys))

(def-store-migration ("Propagate old api-key deletions" :version 19)
  "See T1182"
  (dolist (api-key (bknr.datastore:class-instances 'api-key))
    (unless (slot-boundp api-key 'api-key)
      (delete-api-key api-key))))

(def-store-migration ("Add default permissions" :version 24)
  "See T1388"
  (let ((defaults (uiop:call-function "core/api/dashboard/api-keys::%default-permissions")))
    (dolist (api-key (bknr.datastore:class-instances 'api-key))
      (unless (slot-boundp api-key '%permissions)
        (setf (api-key-permissions api-key)
              defaults)))))

;; (encode-api-token (elt (bknr.datastore:class-instances 'api-key) 10))
