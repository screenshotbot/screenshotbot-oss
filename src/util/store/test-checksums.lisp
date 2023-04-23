;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-checksums
  (:use #:cl
        #:fiveam)
  (:import-from #:flexi-streams
                #:get-output-stream-sequence
                #:make-in-memory-input-stream
                #:make-in-memory-output-stream)
  (:import-from #:util/store/checksums
                #:end-of-file-error
                #:checksum-failure
                #:encode-checksumed-object)
  (:import-from #:bknr.datastore
                #:decode)
  (:import-from #:util/store/object-id
                #:make-oid))
(in-package :util/store/test-checksums)

(util/fiveam:def-suite)

(def-fixture state ()
  (let ((output (make-in-memory-output-stream))
        (test-oid (make-oid :arr #(1 2 3 4 5 6 7 8 9 10 11 12))))
    (&body)))

(test encode-and-decode
  (with-fixture state ()
    (encode-checksumed-object test-oid output)
    (is (equalp test-oid
                (decode
                 (make-in-memory-input-stream (get-output-stream-sequence output)))))))

(test checksum-failure-condition
  (with-fixture state ()
    (encode-checksumed-object test-oid output)
    (let ((buff (get-output-stream-sequence output)))
      (setf (elt buff 10) 3)
      (signals checksum-failure
        (decode (make-in-memory-input-stream buff))))))


(test checksum-failure-condition-when-checksum-is-tweaked
  (with-fixture state ()
    (encode-checksumed-object test-oid output)
    (let ((buff (get-output-stream-sequence output)))
      (setf (elt buff 4) 3)
      (signals checksum-failure
        (decode (make-in-memory-input-stream buff))))))


(test truncated-file
  (with-fixture state ()
    (encode-checksumed-object test-oid output)
    (let ((buff (get-output-stream-sequence output)))
      (adjust-array buff (1- (length buff))
                    :fill-pointer (1- (length buff)))
      (setf (elt buff 4) 3)
      (signals end-of-file-error
        (decode (make-in-memory-input-stream buff))))))
