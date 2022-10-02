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
  (let ((stream (response-stream response)))
    (when (open-stream-p stream)
      (finish-output stream)
      (close stream)))
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
             (finish-output stream)
             ;; this close is required.. not sure why though
             (close stream))))
    (cleanup async-response)))

(defun hex:handle-async-error (async-response &key (code 500)
                                                (message "Something went wrong internally."))
  ;; We don't have prebuilt headers to work with here, so let's mock
  ;; the HTTP response.
  (unwind-protect
       (let ((stream (flex:make-flexi-stream
                      (response-stream async-response))))
         (format stream "HTTP/1.1 ~a Internal Server Error~C~C" code
                 #\Return #\Linefeed)
         (format stream "Content-Length: ~a~C~C~C~C" (length message)
                 #\Return #\Linefeed
                 #\Return #\Linefeed)
         (format stream "~a" message)
         (finish-output stream))
    (cleanup async-response)))
