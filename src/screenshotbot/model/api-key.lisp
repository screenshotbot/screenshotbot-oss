;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/api-key
  (:use :cl)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object
                #:store-object-id
                #:with-transaction)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:screenshotbot/api-key-api
                #:api-key
                #:api-key-key
                #:api-key-secret-key
                #:delete-api-key)
  (:import-from #:screenshotbot/model/core
                #:generate-api-key
                #:generate-api-secret)
  (:import-from #:screenshotbot/user-api
                #:api-key-company
                #:api-key-user
                #:user-api-keys)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:util/misc
                #:make-mp-hash-table)
  (:import-from #:util/store/fset-index
                #:fset-set-index
                #:index-least)
  (:import-from #:util/store/store
                #:defindex
                #:with-class-validation)
  (:export
   #:%find-api-key
   #:all-api-keys
   #:api-key
   #:api-key-company
   #:api-key-description
   #:api-key-key
   #:api-key-secret-key
   #:api-key-user
   #:delete-api-key
   #:make-transient-key
   #:render-api-token))
(in-package :screenshotbot/model/api-key)

(defindex +expires-index+
  'fset-set-index
  :slot-name 'expires-at)

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
      :index-type unique-index
      :index-initargs (:test #'equal)
      :index-reader %%find-api-key
      :index-values all-api-keys
      :initform nil)
     (api-secret-key
      :type string
      :initarg :api-secret-key
      :accessor api-key-secret-key
      :initform nil)
     (%description
      :type string
      :initarg :description
      :initform nil
      :accessor api-key-description)
     (expired-p
      :type boolean
      :reader expired-p
      :initform nil))
    (:metaclass persistent-class)
    (:default-initargs
     :api-key (generate-api-key)
     :api-secret-key (generate-api-secret))))

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
   (created-at :initform (get-universal-time)))
  (:documentation "A transient key generated for communication between
  the replay service and this server."))

(defmethod expired-p ((self transient-api-key))
  nil)

(defun %find-api-key (str)
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


(defmethod render-api-token ((self api-key))
  (format nil "cli-~a:~a"
          (store-object-id self)
          (api-key-secret-key self)))


(defmethod delete-api-key ((api-key api-key))
  ;; uggh, what's the right way to delete this?
  (with-transaction ()
    (slot-makunbound api-key 'api-key)))

(defmethod user-api-keys (user company)
  (loop for api-key in (bknr.datastore:class-instances 'api-key)
        if (and (eq user (api-key-user api-key))
                (slot-boundp api-key 'api-key) ;; See T929
                (eq company (api-key-company api-key)))
          collect api-key))

(defmethod cleanup-expired-api-keys ()
  (let ((next (index-least +expires-index+))
        (now (get-universal-time)))
    (when (and next (< (expires-at next) now))
      (bknr.datastore:delete-object next)
      (cleanup-expired-api-keys))))

(def-cron cleanup-expired-api-keys (:step-min 5)
  (cleanup-expired-api-keys))
