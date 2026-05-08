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
                #:initialize-transient-instance
                #:persistent-class
                #:store-object)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:core/config/api
                #:validate
                #:config)
  (:export
   #:on-config-changed))
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

(define-condition value-must-be-string (error)
  ())

(defgeneric on-config-changed (key value)
  (:documentation "Called any time a config is changed. The key will be the interned
value of the string in the keword package. So 'foo.bar' will become
:FOO.BAR. Use a selector on the key to
select a specific config. This will also be called during store reload.")
  (:method (key value)
    (values)))

(defmethod initialize-transient-instance :after ((self config-setting))
  (on-config-changed
   (intern (string-upcase (config-setting-key self)) :keyword)
   (config-setting-value self)))

(defmethod (setf config) (value key)
  (error 'value-must-be-string))

(defmethod (setf config) ((value string) key)
  (handler-bind ((error (lambda (e)
                          (format t "Got error: ~a~%" e))))
    (validate (intern (str:upcase key) :keyword) value))
  (?. bknr.datastore:delete-object (config-setting-for-key key))
  (make-instance 'config-setting
                 :key key
                 :value value))




