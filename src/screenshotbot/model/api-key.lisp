;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/api-key
  (:use #:cl
        #:alexandria
        #:screenshotbot/model/core
        #:screenshotbot/user-api
        #:screenshotbot/api-key-api)
  (:import-from #:bknr.datastore
                #:store-object
                #:store-object-id
                #:with-transaction
                #:unique-index
                #:persistent-class)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:util/misc
                #:make-mp-hash-table)
  (:export
   #:api-key
   #:%find-api-key
   #:api-key-user
   #:all-api-keys
   #:api-key-company
   #:delete-api-key
   #:make-transient-key
   #:api-key-key
   #:api-key-secret-key))
(in-package :screenshotbot/model/api-key)

(with-class-validation
  (defclass api-key (store-object)
    ((user
      :type user
      :initarg :user
      :accessor api-key-user)
     (company
      :type company
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
     (expired-p
      :type boolean
      :initform nil))
    (:metaclass persistent-class)
    (:default-initargs
     :api-key (generate-api-key)
     :api-secret-key (generate-api-secret))))

(defvar *transient-keys* (make-mp-hash-table :test #'equal))

(defclass transient-api-key ()
  ((api-key
    :initform (generate-api-key)
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

(defun %find-api-key (str)
  (or
   (%%find-api-key str)
   (gethash str *transient-keys*)))

(defun make-transient-key (&key user company)
  (let ((key (make-instance 'transient-api-key
                             :user user
                             :company company)))
    (setf (gethash (api-key-key key) *transient-keys*)
          key)
    key))

;; TODO: delete
(defmethod initialize-instance :around ((obj api-key)
                                        &rest args
                                        &key api-key api-secret-key
                                        &allow-other-keys)
  (call-next-method))

(defmethod model-id ((api-key api-key))
  (store-object-id api-key))


(defmethod delete-api-key ((api-key api-key))
  ;; uggh, what's the right way to delete this?
  (with-transaction ()
    (slot-makunbound api-key 'api-key)))

(defmethod user-api-keys ((user user) (company company))
  (loop for api-key in (all-api-keys)
        if (and (eq user (api-key-user api-key))
                (eq company (api-key-company api-key)))
          collect api-key))
