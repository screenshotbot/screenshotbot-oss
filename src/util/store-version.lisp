;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store-version
  (:use #:cl)
  (:import-from #:bknr.datastore
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

(defvar *store-version* 1
  "The current version of the store being used.")

(defvar *min-store-version* 1
  "The minimum supported version of the store, for loading purposes.")

(defclass version-subsystem ()
  ((store-version :accessor store-version
                  :initarg :store-version
                  :documentation "The version of the current store."))
  (:documentation "A subsystem that also saves the store-version, and verifies the store
version on loads"))

(defmethod initialize-subsystem :after ((self version-subsystem) store store-existed-p)
  (cond
    (store-existed-p
     (setf (store-version self)
           (read-current-version (ensure-store-current-directory store))))
    (t
     (setf (store-version self) *store-version*)
     (write-current-version (ensure-store-current-directory store)))))


(defmethod restore-subsystem (store (self version-subsystem) &key until)
  (declare (ignore until)))

(defmethod snapshot-subsystem (store (self version-subsystem))
  (write-current-version (ensure-store-current-directory store)))

(defmethod read-current-version (current-dir)
  (let ((ver-file (path:catfile current-dir "store-version")))
    (if (path:-e ver-file)
        (parse-integer (str:trim (uiop:read-file-string ver-file)))
        0)))

(defmethod write-current-version (current-dir)
  (with-open-file (store-version (path:catfile current-dir "store-version")
                                 :direction :output
                                 :if-exists :supersede)
    (format store-version "~a" *store-version*)))

(defsubsystem version-subsystem :priority 5)
