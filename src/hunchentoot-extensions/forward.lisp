;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :hunchentoot-extensions/forward
  (:use #:cl)
  (:import-from #:hex
                #:forward-request)
  (:import-from #:alexandria
                #:when-let
                #:assoc-value))
(in-package :hunchentoot-extensions/forward)


(auto-restart:with-auto-restart ()
  (defun forward-request (url &key (request hunchentoot:*request*)
                                (keep-current-host nil))
    "Forward the current request to the destination URL. If
keep-current-host is T, then we'll forward to the destination, but
keep the Host header the same."
    ;; We'll avoid using util/http-request for now. It adds a depedency,
    ;; but probably we want a lot more control over the behavior.
    (let* ((body (hunchentoot:raw-post-data :force-binary t))
           (uri (quri:uri url))
           (dest-uri (quri:render-uri
                      (quri:make-uri
                       :scheme (quri:uri-scheme uri)
                       :host (if keep-current-host
                                 (cl-ppcre:regex-replace-all ":[0-9]*$" (hunchentoot:host request) "")
                                 (quri:uri-host uri))
                       :port (quri:uri-port uri)
                       :defaults (quri:uri (hunchentoot:request-uri request))))))
      (multiple-value-bind (response code headers)
          (drakma:http-request
           dest-uri
           :additional-headers (list*
                                (cons :x-forwarded-host (hunchentoot:host request))
                                (loop for (key . value) in (hunchentoot:headers-in request)
                                      unless (member key '(:content-length :user-agent :host
                                                           :|:AUTHORITY:|
                                                           :|:METHOD:|
                                                           :|:PATH:|
                                                           :|:SCHEME:|
                                                           :accept-encoding))
                                        collect (cons key value)))
           :method (hunchentoot:Request-method request)
           :real-host (quri:uri-host uri)
           :force-binary t
           :want-stream t
           :decode-content nil
           :redirect nil
           :user-agent (hunchentoot:header-in :User-agent request)
           :content-type (hunchentoot:header-in :content-type request)
           :content body)
        (setf (hunchentoot:return-code*) code)

        (loop for (key . value) in headers
              if (not (member key '(:content-length) ))
                do (setf (hunchentoot:header-out key) value))

        (when-let ((Content-length (assoc-value headers :content-length)))
          (setf (hunchentoot:header-out :content-length)
                (parse-integer content-length)))

        (setf (hunchentoot:header-out :x-final-code) code)
        (Setf (hunchentoot:header-out :x-final-headers) (format nil "~a" headers))
        (setf (hunchentoot:Header-out :x-dest-uri) (format nil "~a" dest-uri))

        (let* ((stream (hunchentoot:send-headers)))
          (uiop:copy-stream-to-stream response stream
                                      :element-type 'flex:octet)
          (finish-output stream)
          (hunchentoot:abort-request-handler))))))
