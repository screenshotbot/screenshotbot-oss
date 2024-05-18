;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/store-version
  (:nicknames :util/store-version)
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:restore-subsystem
                #:snapshot-subsystem
                #:store-subsystem-snapshot-pathname
                #:snapshot-store)
  (:import-from #:bknr.datastore
                #:store-current-directory)
  (:import-from #:bknr.datastore
                #:ensure-store-current-directory)
  (:import-from #:bknr.datastore
                #:restore-subsystem)
  (:import-from #:bknr.datastore
                #:initialize-subsystem)
  (:import-from #:bknr.datastore
                #:store-directory)
  (:import-from #:bknr.datastore
                #:snapshot-subsystem)
  (:import-from #:util/store
                #:defsubsystem))
(in-package :util/store-version)

(defparameter *store-version* 16
  "The current version of the store being used.")

(defvar *snapshot-store-version* *store-version*
  "The version of the snapshot that was read in. This is the one we
migrate.")

(defvar *min-store-version* 1
  "The minimum supported version of the store, for loading purposes.")

(defclass version-subsystem ()
  ()
  (:documentation "A subsystem that also saves the store-version, and verifies the store
version on loads"))

(defmethod initialize-subsystem :after ((self version-subsystem) store store-existed-p)
  (log:info "Store directory: ~a" (bknr.datastore::store-directory store))
  (setf *snapshot-store-version* *store-version*))


(defmethod restore-subsystem (store (self version-subsystem) &key until)
  (declare (ignore until))
  (setf *snapshot-store-version*
        (or
         (read-current-version store self)
         *store-version*)))

(defmethod snapshot-subsystem (store (self version-subsystem))
  (write-current-version store self))

(defmethod read-current-version (store self)
  (let ((snapshot-pathname (store-subsystem-snapshot-pathname store self)))
    (loop for ver-file in (list snapshot-pathname
                                ;; old version:
                                (make-pathname :name "store-version" :defaults snapshot-pathname))
          if (path:-e ver-file)
            return (parse-integer (str:trim (uiop:read-file-string ver-file))))))

(defmethod write-current-version (store self)
  (with-open-file (store-version (ensure-directories-exist
                                  (store-subsystem-snapshot-pathname store self))
                                 :direction :output
                                 :if-exists :supersede)
    (format store-version "~a" *snapshot-store-version*)))

(defsubsystem version-subsystem :priority 5)
