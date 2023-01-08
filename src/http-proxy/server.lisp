;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :http-proxy/server
  (:use #:cl)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:util/lru-cache
                #:with-cache-file
                #:lru-cache))
(in-package :http-proxy/server)

(defclass http-proxy (hunchentoot:acceptor)
  ((lru-cache :accessor lru-cache)
   (cache-size :initarg :cache-size
               :initform "4GB"
               :reader cache-size)
   (cache-dir :initform (tmpdir:mkdtemp)
              :reader cache-dir)))

(defparameter *bad-request-headers*
  (list
   :accept-encoding))

(defmethod hunchentoot:start :before ((self http-proxy))
  (log:info "Starting HTTP proxy")
  (setf (lru-cache self)
        (make-instance 'lru-cache
                       :max-size (cache-size self)
                       :dir (ensure-directories-exist (cache-dir self)))))

(defmethod hunchentoot:stop :after ((self http-proxy) &key &allow-other-keys)
  (log:info "Stopping HTTP proxy")
  (tmpdir::%delete-directory (cache-dir self)))


(defun fix-proxy-headers (headers)
  (loop for (key . value) in headers
        unless (member key *bad-request-headers*)
          collect (cons key value)))

(defparameter *bad-headers*
  (list
   :content-length
   :content-encoding
   :keep-alive
   :close
   :connection
   :transfer-encoding))

(defmethod cache-key ((self http-proxy) uri)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence 'ironclad:sha1 (flex:string-to-octets uri))))

(defmethod make-forward-request ((self http-proxy)
                                 uri output-file
                                 &rest args &key &allow-other-keys)
  "Like util/request:http-request, but returns a file instead of a stream
for the content."
  (multiple-value-bind (data ret headers)
      (apply #'util/request:http-request
             (quri:render-uri uri)
             args)
    (with-open-file (out-stream output-file :direction :output
                                            :if-exists :supersede
                                            :element-type 'flex:octet)
      (write-sequence data out-stream)
      (values ret headers))))

(defmethod hunchentoot:acceptor-dispatch-request ((self http-proxy)
                                                  request)
  (cond
    ((member (hunchentoot:request-method request)
             '(:connect :post :put :delete))
     (setf (hunchentoot:return-code hunchentoot:*reply*) 403)
     "Only GET requests are allowed with this proxy")
    (t
     (assert (eql :get (hunchentoot:request-method request)))
     (let ((uri (quri:merge-uris
                 (hunchentoot:request-uri request)
                 (format nil "http://~a"(hunchentoot:host request)))))
       (log:info "Requesting: ~a" uri)
       (with-cache-file (tmp-file (lru-cache self) (cache-key self (quri:render-uri uri)))
         (multiple-value-bind (ret headers)
             (make-forward-request self
                                   uri
                                   tmp-file
                                   :additional-headers (fix-proxy-headers (hunchentoot:headers-in request))
                                   :want-stream nil
                                   :keep-alive nil
                                   :close t
                                   :redirect nil
                                   :force-binary t)
           (hunchentoot:handle-static-file
            tmp-file
            (assoc-value headers :content-type)
            (lambda (pathname content-type)
              (declare (ignore pathname content-type))
              (loop for (key . value) in headers
                    unless
                    (member key *bad-headers*)
                    do (setf (hunchentoot:header-out key) value))
              (setf (hunchentoot:return-code hunchentoot:*reply*) ret)))
           (log:info "Finished streaming response for ~a" uri))))))
)

#+nil
(setf *acceptor* (make-instance 'http-proxy :port 3127))

#+nil
(hunchentoot:start *acceptor*)
