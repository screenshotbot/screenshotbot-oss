;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/unlikely-to-change-snapshot
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:encode
                #:class-layout-slots))
(in-package :util/store/unlikely-to-change-snapshot)

(defclass unlikely-to-change-snapshot ()
  ((object :initarg :object
           :reader %object))
  (:documentation "An object snapshot, indicating that the object is unlikely to change
during a background snapshot. If the object did change since the
object was created, then the background snapshot process crashes"))

(define-condition object-changed-during-snapshot (error)
  ())

(defmethod bknr.datastore:encode-slots-for-object (class-layout (self unlikely-to-change-snapshot)
                                                   stream
                                                   &key changedp)
  (log:info "Got changedp: ~a" changedp)
  (cond
    (changedp
     (error 'object-changed-during-snapshot))
    (t
     (let ((object (%object self)))
       (loop for slot in (class-layout-slots class-layout)
             do (encode
                 (cond
                   ((slot-boundp object slot)
                    (slot-value object slot))
                   (t
                    'bknr.datastore::unbound))
                 stream))))))


