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
                #:make-in-memory-input-stream)
  (:import-from #:bknr.datastore
                #:transaction)
  (:import-from #:util/store/store
                #:checksumed-mp-store)
  (:import-from #:bknr.datastore
                #:restore-transaction-log))
(in-package :util/store/checksums)


(define-condition base-error (error)
  ())

(define-condition end-of-file-error (base-error)
  ())

(define-condition could-not-read-length (end-of-file-error)
  ())

(define-condition could-not-read-checksum (end-of-file-error)
  ())

(define-condition checksum-failure (base-error)
  ())

(defmethod decode-object ((tag (eql #\C)) stream)
  (let ((length
          (handler-case
              (decode stream)
            (end-of-file ()
              (error 'could-not-read-length))))
        (digest (make-array 4 :element-type '(unsigned-byte 8))))
    (unless (= 4 (read-sequence digest stream))
      (error 'could-not-read-checksum))

    (let ((buff (make-array length :element-type '(unsigned-byte 8))))
      (let ((bytes-read (read-sequence buff stream)))
        (when (< bytes-read length)
          (error 'end-of-file-error)))
      (let ((actual-digest (ironclad:digest-sequence :crc32 buff)))
        (unless (equalp digest actual-digest)
          (error 'checksum-failure)))
      (let ((stream (make-in-memory-input-stream buff)))
       (decode stream)))))

(defmethod encode-checksumed-object (object stream &optional
                                                     (next-method (lambda (object stream)
                                                                    (encode-object object stream))))
  (%write-tag #\C stream)
  (let ((tmp (flex:make-in-memory-output-stream)))
    (funcall next-method object tmp)
    (let ((buff (flex:get-output-stream-sequence tmp)))
      (encode-integer (length buff) stream)
      (let ((digest (ironclad:digest-sequence :crc32 buff)))
        (write-sequence digest stream))
      (write-sequence buff stream))))


(defmethod encode-object :around ((transaction transaction) stream)
  (encode-checksumed-object transaction stream
                            (lambda (transaction stream)
                              (call-next-method transaction stream))))

(defmethod restore-transaction-log :around ((store checksumed-mp-store)
                                            transaction-log
                                            &key until)
  (declare (ignore until))
  (handler-bind ((base-error (lambda (e)
                               (declare (ignore e))
                               (invoke-restart 'bknr.datastore::discard))))
      (call-next-method)))

