;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/downloadable-run
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:blob-pathname
                #:persistent-class
                #:blob)
  (:import-from #:util/store/store
                #:defindex
                #:with-class-validation)
  (:import-from #:util/store/fset-index
                #:fset-unique-index)
  (:import-from #:screenshotbot/user-api
                #:screenshot-name
                #:recorder-run-screenshots)
  (:import-from #:screenshotbot/model/image
                #:with-local-image))
(in-package :screenshotbot/model/downloadable-run)

(defindex +run-index+
  'fset-unique-index
  :slot-name '%run)

(defvar *lock* (bt:make-lock))

(with-class-validation
  (defclass downloadable-run (blob)
    ((%run :initarg :run
           :index +run-index+
           :index-reader %downloadable-run-for-run
           :reader %run)
     (%created-at
      :initarg :created-at ;; for tests
      :reader created-at))
    (:metaclass persistent-class)
    (:default-initargs :created-at (get-universal-time))
    (:documentation "Represents a downloadable artifact, a zip/tar of all of the
screenshots in a run.")))

(defun find-or-create-downloadable-run (run)
  (bt:with-lock-held (*lock*)
    (or (%downloadable-run-for-run run)
        (make-instance 'downloadable-run :run run))))

(defmethod build-archive ((self downloadable-run))
  (zip:with-output-to-zipfile (zip-writer (blob-pathname self) :if-exists :supersede)
    (dolist (screenshot (recorder-run-screenshots (%run self)))
      (with-local-image (file screenshot)
        (with-open-file (data file :direction :input :element-type '(unsigned-byte 8))
          (zip:write-zipentry zip-writer
                              (format nil "~a.png" (screenshot-name screenshot))
                              data))))))

(defun delete-old-runs ()
  (let ((boundary (- (get-universal-time) (* 24 3600 7))))
    (loop for dr in (bknr.datastore:class-instances 'downloadable-run)
          do
             (bt:with-lock-held (*lock*)
               (when (< (created-at dr) boundary)
                 (when (path:-e (blob-pathname dr))
                   (delete-file (blob-pathname dr)))
                 (bknr.datastore:delete-object dr))))))
