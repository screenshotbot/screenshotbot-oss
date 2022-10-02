;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :hunchentoot-extensions/async
  (:use #:cl
        #:hex)
  (:import-from #:util/macros
                #:def-easy-macro)
  (:import-from #:hunchentoot
                #:reset-connection-stream
                #:*acceptor*)
  (:import-from #:chunga
                #:chunked-output-stream)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:prepare-async-response))
(in-package :hunchentoot-extensions/async)

(defvar *in-progress* (make-hash-table))

(defclass async-response ()
  ((stream :initarg :stream
           :reader response-stream)
   (headers :initarg :headers
            :reader response-headers)))

(def-easy-macro prepare-async-response (&binding async-response &fn body)
  (let* ((stream hunchentoot::*hunchentoot-stream*)
         (headers (flex:with-output-to-sequence (stream)
                    (let ((hunchentoot::*hunchentoot-stream* stream))
                      (hunchentoot:send-headers)
                      (finish-output hunchentoot::*hunchentoot-stream*)
                      (finish-output stream))))
         (response (make-instance 'async-response
                                  :stream stream
                                  :headers headers)))
    (setf (gethash response *in-progress*) t)
    (hunchentoot:detach-socket hunchentoot:*acceptor*)
    (funcall body response)
    (hunchentoot:abort-request-handler)))

(defun cleanup (response)
  (remhash response *in-progress*))

(defun hex:handle-async-static-file (async-response output-file)
  (unwind-protect
       (let ((stream (response-stream async-response))
             (headers (response-headers async-response)))
         (assert (not (typep stream 'chunked-output-stream)))
         (write-sequence headers stream)
         (finish-output stream)
         (let ((stream (chunga:make-chunked-stream stream)))
           (setf (chunga:chunked-stream-output-chunking-p stream) t)
           (with-open-file (file output-file :element-type '(unsigned-byte 8))
             (uiop:copy-stream-to-stream file
                                         stream
                                         :element-type '(unsigned-byte 8))
             (force-output stream)
             (close stream))))
    (cleanup async-response)))
