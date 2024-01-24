;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-encodable
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/encodable
                #:encodable)
  (:import-from #:bknr.datastore
                #:decode
                #:encode
                #:decode-object
                #:encode-object)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that))
(in-package :util/store/test-encodable)


(util/fiveam:def-suite)

(defclass simple-obj (encodable)
  ((slot1 :initarg :slot1)
   (slot2 :initarg :slot2)))

(defun recreate (obj)
  (let ((stream (flex:make-in-memory-output-stream)))
    (encode obj stream)
    (let ((stream (flex:make-in-memory-input-stream
                   (flex:get-output-stream-sequence stream))))
      (decode stream))))

(test preconditions
  (is (equal "foo" (recreate "foo")))
  (assert-that (recreate (make-instance 'simple-obj))
               (has-typep 'simple-obj)))

(test slots-are-saved
  (let ((obj (recreate (make-instance 'simple-obj :slot1 "foo"))))
    (is (equal "foo" (slot-value obj 'slot1)))))
