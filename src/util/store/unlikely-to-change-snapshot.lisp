;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/unlikely-to-change-snapshot
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object-id
                #:encode
                #:class-layout-slots)
  (:export
   #:*unlikely-to-change*))
(in-package :util/store/unlikely-to-change-snapshot)

(defclass unlikely-to-change-snapshot ()
  ()
  (:documentation "An object snapshot, indicating that the object is unlikely to change
during a background snapshot. If the object did change since the
object was created, then the background snapshot process crashes"))

(define-condition object-changed-during-snapshot (error)
  ((object :initarg :object
           :initform nil
           :reader %object)))

(defmethod print-object ((self object-changed-during-snapshot) stream)
  (format stream "Object changed during snapshot: ~a"
          (%object self)))

(defvar *unlikely-to-change* (make-instance 'unlikely-to-change-snapshot))

(defmethod bknr.datastore:encode-slots-for-object (class-layout (self unlikely-to-change-snapshot)
                                                   stream
                                                   &key changedp
                                                     object)
  (cond
    (changedp
     (error 'object-changed-during-snapshot :object object))
    (t
     (loop for slot in (class-layout-slots class-layout)
           do (encode
               (cond
                 ((slot-boundp object slot)
                  (slot-value object slot))
                 (t
                  'bknr.datastore::unbound))
               stream)))))



(defclass unlikely-to-change-mixin ()
  ())

(defmethod bknr.datastore:make-object-snapshot-v2 ((self unlikely-to-change-mixin)
                                                   next-object-id)
  (let ((cutoff (- next-object-id 500000)))
    (when (< (store-object-id self) cutoff)
      *unlikely-to-change*)))
