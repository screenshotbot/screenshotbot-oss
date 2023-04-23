;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/checksums
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:%write-tag)
  (:import-from #:bknr.datastore
                #:encode-object)
  (:import-from #:bknr.datastore
                #:encode)
  (:import-from #:bknr.datastore
                #:decode-object)
  (:import-from #:bknr.datastore
                #:decode)
  (:import-from #:bknr.datastore
                #:encode-integer)
  (:import-from #:bknr.datastore
                #:%decode-integer)
  (:import-from #:flexi-streams
                #:make-in-memory-input-stream))
(in-package :util/store/checksums)


(define-condition base-error (error)
  ())

(define-condition end-of-file-error (base-error)
  ())

(define-condition checksum-failure (base-error)
  ())

(defmethod decode-object ((tag (eql #\C)) stream)
  (let ((length (decode stream))
        (digest (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence digest stream)
    (let ((buff (make-array length :element-type '(unsigned-byte 8))))
      (let ((bytes-read (read-sequence buff stream)))
        (when (< bytes-read length)
          (error 'end-of-file-error)))
      (let ((actual-digest (ironclad:digest-sequence :crc32 buff)))
        (unless (equalp digest actual-digest)
          (error 'checksum-failure)))
      (let ((stream (make-in-memory-input-stream buff)))
       (decode stream)))))

(defmethod encode-checksumed-object (object stream)
  (%write-tag #\C stream)
  (let ((tmp (flex:make-in-memory-output-stream)))
    (encode-object object tmp)
    (let ((buff (flex:get-output-stream-sequence tmp)))
      (encode-integer (length buff) stream)
      (let ((digest (ironclad:digest-sequence :crc32 buff)))
        (write-sequence digest stream))
      (write-sequence buff stream))))
