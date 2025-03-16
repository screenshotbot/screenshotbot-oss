;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/truncated-stream
  (:use #:cl)
  (:local-nicknames #-lispworks
                    (#:stream #:trivial-gray-streams)))
(in-package :util/truncated-stream)

(defclass truncated-stream (stream:fundamental-input-stream)
  ((delegate :initarg :delegate
             :reader delegate)
   (bytes-left :initarg :bytes-left
               :accessor bytes-left))
  (:documentation "A stream that is truncated to BYTES-LEFT bytes. Reading beyond this
will return EOF. Closing the stream closes the underlying stream."))

(defmethod stream::stream-element-type ((self truncated-stream))
  (stream::stream-element-type (delegate self)))

(defmethod stream:stream-read-sequence ((self truncated-stream)
                                        sequence
                                        start
                                        end
                                        #-lispworks #-lispworks
                                                    &key &allow-other-keys)
  (cond
    ((= 0 (bytes-left self))
     start)
    (t
     (let ((result (stream:stream-read-sequence (delegate self) sequence
                                                start
                                                (min
                                                 end
                                                 (+ start (bytes-left self))))))
       (decf (bytes-left self)
             (- result start))
       result))))


(defmethod stream:stream-finish-output ((self truncated-stream))
  (log:debug "finishing output")
  (stream:stream-finish-output (delegate self)))

(defmethod stream:stream-force-output ((self truncated-stream))
  (log:debug "forcing output")
  (stream:stream-force-output (delegate self)))





