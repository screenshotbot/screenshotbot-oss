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
                #:?.))
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

(defun config (key)
  (?. config-setting-value (config-setting-for-key key)))

(defun (setf config) (value key)
  (?. bknr.datastore:delete-object (config-setting-for-key key))
  (make-instance 'config-setting
                 :key key
                 :value value))


