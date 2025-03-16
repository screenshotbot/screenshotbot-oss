;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-truncated-stream
  (:use #:cl
        #:fiveam)
  (:import-from #:util/truncated-stream
                #:truncated-stream))
(in-package :util/tests/test-truncated-stream)


(util/fiveam:def-suite)

(def-fixture state ()
  (let* ((input (make-array 100 :initial-element 0 :element-type '(unsigned-byte 8)))
         (input-stream (flex:make-in-memory-input-stream input)))
    (&body)))

(test simple-invocation ()
  (with-fixture state ()
    (let ((stream (make-instance
                   'truncated-stream
                   :delegate input-stream
                   :bytes-left 10)))
      (let* ((output (make-array 200)))
        (is (eql 10 (read-sequence output stream)))))))

(test longer-truncation-than-data ()
  (with-fixture state ()
    (let ((stream (make-instance
                   'truncated-stream
                   :delegate input-stream
                   :bytes-left 1000)))
      (let* ((output (make-array 200)))
        (is (eql 100 (read-sequence output stream)))))))

(test double-invocation ()
  (with-fixture state ()
    (let ((stream (make-instance
                   'truncated-stream
                   :delegate input-stream
                   :bytes-left 10)))
      (let* ((output (make-array 3)))
        (is (eql 3 (read-sequence output stream)))
        (is (eql 3 (read-sequence output stream)))
        (is (eql 3 (read-sequence output stream)))
        (is (eql 1 (read-sequence output stream)))))))
