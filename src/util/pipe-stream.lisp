;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/pipe-stream
  (:use #:cl)
  (:import-from #:trivial-gray-streams
                #:stream-read-byte
                #:stream-write-byte
                #:fundamental-binary-output-stream
                #:fundamental-binary-input-stream)
  (:export
   #:in-memory-pipe-stream))
(in-package :util/pipe-stream)

(defclass in-memory-pipe-stream (fundamental-binary-input-stream
                                 fundamental-binary-output-stream)
  ((vector :reader pipe-stream-vector
           :initform (make-array 0
                                 :adjustable t
                                 :fill-pointer 0
                                 :element-type '(unsigned-byte 8)))
   (read-ptr :initform 0
             :accessor read-ptr))
  (:documentation "An in-memory binary IO stream. Writing appends to the end, reading
reads from the beginning. This is currently intended to be used for
testing, so it's not really optimized."))

(defmethod stream-write-byte ((stream in-memory-pipe-stream) byte)
  (vector-push-extend byte (pipe-stream-vector stream)))

(defmethod stream-read-byte ((stream in-memory-pipe-stream))
  (cond
    ((= (read-ptr stream)
        (length (pipe-stream-vector stream)))
     :eof)
    (t
     (prog1
         (aref (pipe-stream-vector stream)
               (read-ptr stream))
       (incf (read-ptr stream))))))

(defmethod stream-element-type ((stream in-memory-pipe-stream))
  '(unsigned-byte 8))
