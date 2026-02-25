;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/config/model
  (:use #:cl)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:core/config/api
                #:validate
                #:config))
(in-package :core/config/model)

(defindex +keys+
  'unique-index
  :test 'equal
  :slot-name '%key)

(defclass config-setting (store-object)
  ((%key :initarg :key
         :initform nil
         :reader config-setting-key
         :index +keys+
         :index-reader config-setting-for-key
         :documentation "A string value for the config setting (it's a string because humans have to configure it.")
   (%value :initarg :value
          :initform nil
           :reader config-setting-value))
  (:metaclass persistent-class))

(defmethod config (key)
  (?. config-setting-value (config-setting-for-key key)))

(defmethod (setf config) (value key)
  (handler-bind ((error (lambda (e)
                          (format t "Got error: ~a~%" e))))
    (validate (intern (str:upcase key) :keyword) value))
  (?. bknr.datastore:delete-object (config-setting-for-key key))
  (make-instance 'config-setting
                 :key key
                 :value value))




