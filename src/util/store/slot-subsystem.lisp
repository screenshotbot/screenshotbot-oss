;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/slot-subsystem
  (:use #:cl)
  (:import-from #:util/store/store
                #:defsubsystem)
  (:import-from #:bknr.datastore
                #:decode
                #:store-subsystem-snapshot-pathname
                #:encode
                #:snapshot-subsystem-async
                #:deftransaction
                #:restore-subsystem))
(in-package :util/store/slot-subsystem)

(defvar *map* (fset:empty-map)
  "A map from (cons obj slot) to object")

(defvar *lock* (bt:make-lock))

(defclass slot-subsystem ()
  ()
  (:documentation "A subsystem that stores slots values using a functional maps. This is
O(log (nm)) access time, but it allows for very fast snapshots."))

(defsubsystem slot-subsystem
  ;; Load after store-object-subsystem, because we'll reference store
  ;; objects.
  :priority 12)

(defmethod externalized-slot-value (obj slot-name)
  (fset:lookup *map*
               (cons obj slot-name)))

(deftransaction tx-set-slot-value (value obj slot-name)
  (bt:with-lock-held (*lock*)
   (setf
    *map*
    (fset:with
     *map*
     (cons obj slot-name)
     value))))

(defmethod (setf externalized-slot-value) (value obj slot-name)
  (tx-set-slot-value value obj slot-name))

(defmethod restore-subsystem ((store bknr.datastore:store)
                              (self slot-subsystem)
                              &key until)
  (declare (ignore until))
  (setf *map* (fset:empty-map))
  (let ((pathname (store-subsystem-snapshot-pathname store self)))
    (cond
      ((probe-file pathname)
       (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
         ;; read the header 
         (decode stream)
         (setf *map* (decode stream))))
      (t
       (setf *map* (fset:empty-map))))))

(defmethod snapshot-subsystem-async ((store bknr.datastore:store)
                                     (self slot-subsystem))
  (let ((copy *map*)
        (pathname (store-subsystem-snapshot-pathname store self)))
    (lambda ()
      (with-open-file (stream pathname :direction :output :element-type '(unsigned-byte 8)
                                       :if-exists :supersede)
        (encode `(:version 1) stream)
        (encode copy stream)))))



