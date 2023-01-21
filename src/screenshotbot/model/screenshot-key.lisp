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
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot-masks
                #:fake-screenshot)
  (:import-from #:screenshotbot/user-api
                #:screenshot-name)
  (:import-from #:screenshotbot/report-api
                #:screenshot-device
                #:screenshot-lang))
(in-package :screenshotbot/model/screenshot-key)

(defvar *lock* (bt:make-lock))

(with-class-validation
  (defclass screenshot-key (store-object)
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
    (bt:with-lock-held (*lock*)
      (find-existing)
      (make-instance 'screenshot-key
                     :name name
                     :lang lang
                     :device device
                     :masks masks)))))

(defun make-screenshot-from-key (key image)
  (make-instance 'fake-screenshot
                 :name (screenshot-name key)
                 :lang (screenshot-lang key)
                 :device (screenshot-device key)
                 :masks (screenshot-masks key)
                 :image image))

(defun make-key-from-screenshot (screenshot)
  (ensure-screenshot-key
   :name (screenshot-name screenshot)
   :lang (screenshot-lang screenshot)
   :device (screenshot-device screenshot)
   :masks (screenshot-masks screenshot)))
