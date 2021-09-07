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
  (:export
   #:api-key
   #:%find-api-key
   #:api-key-user
   #:all-api-keys
   #:api-key-company
   #:delete-api-key))
(in-package :screenshotbot/model/api-key)

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
    :index-reader %find-api-key
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
  (:metaclass persistent-class))

(defmethod initialize-instance :around ((obj api-key)
                                        &rest args
                                        &key api-key api-secret-key
                                        &allow-other-keys)
  (apply #'call-next-method
         obj
         :api-key (or api-key (generate-api-key))
         :api-secret-key (or api-secret-key (generate-api-secret))
         args))

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
