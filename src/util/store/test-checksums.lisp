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
                #:could-not-read-checksum
                #:could-not-read-length
                #:end-of-file-error
                #:checksum-failure
                #:encode-checksumed-object)
  (:import-from #:bknr.datastore
                #:decode)
  (:import-from #:util/store/object-id
                #:make-oid)
  (:import-from #:bknr.datastore
                #:%write-tag)
  (:import-from #:bknr.datastore
                #:encode-integer)
  (:import-from #:bknr.datastore
                #:%encode-integer))
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
      (setf (elt buff 11) (mod (1+ (elt buff 11)) 128))
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

(test truncated-length
  "When the length itself gets truncated"
  (let ((stream (make-in-memory-input-stream #(67 #| C |#))))
    (signals could-not-read-length
      (decode stream))))

(test truncated-crc32
  "When the length itself gets truncated"
  (let ((builder (make-in-memory-output-stream)))
    (%write-tag #\C builder)
    (%encode-integer 10 builder)

    ;; Write a partial CRC32
    (write-byte 32 builder)
    (write-byte 42 builder)
   (let ((stream (make-in-memory-input-stream (get-output-stream-sequence builder))))
     (signals could-not-read-checksum
       (decode stream)))))
