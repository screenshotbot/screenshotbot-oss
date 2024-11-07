;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/constant-string
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:deftransaction
                #:persistent-class
                #:store-object)
  (:import-from #:bknr.indices
                #:unique-index)
  (:import-from #:util/store/store
                #:defindex
                #:with-class-validation)
  (:import-from #:util/lists
                #:with-batches))
(in-package :screenshotbot/model/constant-string)

(defvar *lock* (bt:make-lock))

(defindex +string-index+
  'unique-index
  :slot-name '%str
  :test #'equal)

;; WARNING: It is unsafe to use CONSTANT-STRING as a key in an
;; fset-index.  When loading from disk, the object will not be ready
;; and so the index will be invalid. Perhaps we can make it sort by
;; index. ins

(with-class-validation
  (defclass constant-string (store-object)
    ((%str :type string
           :initarg :str
           :index-initargs (:test #'equal)
           :index +string-index+
           :index-reader constant-string-with-str
           :reader constant-string-string))
    (:metaclass persistent-class)))

(defmethod constant-string ((str string))
  (or
   (constant-string-with-str str)
   (bt:with-lock-held (*lock*)
     (%constant-string-without-lock str))))

(defun %constant-string-without-lock (str)
  (or
   (constant-string-with-str str)
   (make-instance 'constant-string :str str)))

(defmethod constant-string ((str constant-string))
  str)

(defmethod constant-string ((str null))
  nil)

(defmethod constant-string-string ((str string))
  "Convenience during migrations."
  str)

(defmethod constant-string-string ((str null))
  str)

(defmethod print-object ((str constant-string)
                         stream)
  (print-object (constant-string-string str)
                stream))


(defmethod fset:compare ((self constant-string) two)
  (fset:compare
   (constant-string-string self)
   two))

(defmethod fset:compare ((one string) (two constant-string))
  (fset:compare
   one
   (constant-string-string two)))

(defmethod fset:compare ((one constant-string) (two constant-string))
  (fset:compare
   (constant-string-string one)
   (constant-string-string two)))

(defmethod fset:compare ((self constant-string) (two store-object))
  :unequal)

(defmethod fset:compare ((two store-object) (self constant-string))
  :unequal)

(deftransaction tx-ensure-slot-constant-string (objs slot-name)
  (dolist (obj objs)
    (let ((val (slot-value obj slot-name)))
      (when (typep val 'string)
        (setf (slot-value obj slot-name)
              (%constant-string-without-lock val))))))

(defun ensure-slot-constant-string (objs slot-name)
  (with-batches (objs objs :batch-size 1000)
    (bt:with-lock-held (*lock*)
      (tx-ensure-slot-constant-string objs slot-name))))

