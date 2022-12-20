;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-pipe-stream
  (:use #:cl
        #:fiveam)
  (:import-from #:util/pipe-stream
                #:in-memory-pipe-stream))
(in-package :util/tests/test-pipe-stream)

(util/fiveam:def-suite)

(def-fixture state ()
  (let ((stream (make-instance 'in-memory-pipe-stream)))
    (&body)))

(test simple-read-write
  (with-fixture state ()
    (write-sequence #(1 2 3) stream)
    (pass)
    (let ((seq (make-array 3 :element-type 'flex:octet)))
      (read-sequence seq stream)
      (is (equalp #(1 2 3) seq)))))


(test eof
  (with-fixture state ()
    (write-sequence #(1 2 3) stream)
    (close stream)
    (pass)
    (let ((seq (make-array 4 :element-type 'flex:octet
                           :initial-element 42)))
      (eql 3 (read-sequence seq stream))
      (is (equalp #(1 2 3 42) seq)))))

(test read-write-from-multiple-threads
  (with-fixture state ()
    (let ((thread (bt:make-thread
                   (lambda ()
                     (loop for i from 0 to 1000
                           do (write-byte 1 stream))
                     (finish-output stream)
                     (close stream)))))
      (finishes
       (loop for i from 0 to 1000
             do (assert (eql 1 (read-byte stream)))))
      ;;(is (eql :eof (read-byte stream nil :eof)))
      (bt:join-thread thread))))

(test closes-stream
  (with-fixture state ()
    (finishes
      (close stream))))
