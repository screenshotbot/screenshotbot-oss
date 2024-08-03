;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/simple-object-snapshot
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:encode
                #:class-layout-slots)
  (:export
   #:simple-object-snapshot))
(in-package :util/store/simple-object-snapshot)

(defclass simple-object-snapshot ()
  ((object :initarg :object
           :reader %object))
  (:documentation "We assume that the object is immutable, or if it did mutate, that the mutations are reasonable. If a reference is deleted, then the snapshot will crash"))

;; Some of the tests for this are in test-image :/
(defmethod bknr.datastore:encode-slots-for-object (class-layout (self simple-object-snapshot) stream)
  (let ((object (%object self)))
    (loop for slot in (class-layout-slots class-layout)
          do (encode
              (if (slot-boundp object slot)
                  (slot-value object slot)
                  'bknr.datastore::unbound)
              stream))))
