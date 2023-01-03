;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :hunchentoot-extensions/async
  (:use #:cl
        #:hex)
  (:import-from #:easy-macros
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
  "Handle slow HTTP requests 'asynchronously'.

This macro takes a body, and has one binding. The body is executed
 with the binding, which is an async-response object, and after that
 the response thread is aborted.

Within the body, you SHOULD NOT attempt to access or modify any
 hunchentoot data structures. Any headers should've been set before
 this call. The only calls you may do are hex:handle-async-static-file
 and hex:handle-async-error.

You MUST call one of these functions eventually. Forgetting to do so
 will leak the stream. (However, you can go back and debug leaked
 streams in the *in-progress* variable. In the future, we'll add an
 automatic timeout to destroy old async-responses.)

You can call those functions synchronously in the body, or you can
 call it from a different thread after the body is done. Both will
 behave correctly.

The current implementation has limited functionality with what you can
 do with the response. We'll add more if needed in the future."
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
                      (response-stream async-response)
                      :external-format (flex:make-external-format :latin-1 :eol-style :crlf))))
         (format stream "HTTP/1.1 ~a Internal Server Error~%" code)
         (format stream "Content-Length: ~a~%~%" (length message))
         (format stream "~a" message)
         (finish-output stream))
    (cleanup async-response)))
