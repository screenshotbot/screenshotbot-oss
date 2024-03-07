;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/screenshot-key
  (:use #:cl)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class)
  (:import-from #:screenshotbot/model/image
                #:mask=)
  (:import-from #:screenshotbot/user-api
                #:screenshot-name)
  (:import-from #:screenshotbot/report-api
                #:screenshot-device
                #:screenshot-lang)
  (:import-from #:util/hash-lock
                #:with-hash-lock-held
                #:hash-lock)
  (:export
   #:screenshot-masks))
(in-package :screenshotbot/model/screenshot-key)

(defvar *hash-lock* (make-instance 'hash-lock :test #'equal)
  "Hash lock on the screenshot name. If we use a global lock, then we'll
limit the throughput of creating new screenshot-keys to about
200/s. Under load it probably would be worse.")

(defclass abstract-screenshot-key ()
  ())

(with-class-validation
  (defclass screenshot-key (abstract-screenshot-key store-object)
    ((%name :type string
           :initarg :name
           :accessor screenshot-name
           :index-type hash-index
           :index-initargs (:test #'equal)
           :index-reader screenshot-keys-for-name)
     (%lang
      :initarg :lang
      :initform nil
      :accessor screenshot-lang)
     (%device
      :initarg :device
      :initform nil
      :accessor screenshot-device)
     (%masks
      :initarg :masks
      :initform nil
      :accessor screenshot-masks))
    (:metaclass persistent-class)))

(defclass fake-screenshot-key (abstract-screenshot-key)
  ((name :initarg :name
         :initform nil
         :accessor screenshot-name)
   (lang :initarg :lang
         :initform nil
         :accessor screenshot-lang)
   (device :initarg :device
           :initform nil
           :accessor screenshot-device)
   (masks
    :initarg :masks
    :initform nil
    :accessor screenshot-masks)))

(defmethod print-object ((self screenshot-key) out)
  (format out "#<KEY ~a>" (screenshot-name self)))

(defmethod fset:compare ((a abstract-screenshot-key)
                         (b abstract-screenshot-key))
  (let ((a-name (screenshot-name a))
        (b-name (screenshot-name b)))
    (cond
      ((eql a b)
       :equal)
      ((string< a-name b-name)
       :less)
      ((string> a-name b-name)
       :greater)
      (t
       (fset:compare-slots
        a b
        #'screenshot-lang
        #'screenshot-device
        #'screenshot-masks)))))

(defun ensure-screenshot-key (&key name lang device masks)
  (flet ((find-existing ()
           (loop for key in (screenshot-keys-for-name name)
                 if (and
                     (equal (screenshot-lang key) lang)
                     (equal (screenshot-device key) device)
                     (eql (length masks)
                          (length (screenshot-masks key)))
                     (eql 0
                          (loop for m1 in masks
                                for m2 in (screenshot-masks key)
                                unless (mask= m1 m2)
                                  sum 1)))
            return key)))
   (or
    (find-existing)
    (with-hash-lock-held (name *hash-lock*)
      (find-existing)
      (make-instance 'screenshot-key
                     :name name
                     :lang lang
                     :device device
                     :masks masks)))))
