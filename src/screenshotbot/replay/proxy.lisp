;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;; Proxy for getting screenshots, to avoid network bandwidth usage.
;; Not to be confused with the Squid proxy used for browsers to get
;; images.

(defpackage :screenshotbot/replay/proxy
  (:use #:cl)
  (:import-from #:screenshotbot/webdriver/impl
                #:make-driver)
  (:import-from #:screenshotbot/webdriver/screenshot
                #:full-page-screenshot)
  (:import-from #:hunchentoot
                #:*acceptor*)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:*proxy-port*
   #:ensure-proxy))
(in-package :screenshotbot/replay/proxy)

(defclass replay-proxy (hunchentoot:easy-acceptor)
  ((cache-dir :initarg :cache-dir
              :reader cache-dir
              :initform (pathname "~/screenshot-proxy-cache/")))
  (:default-initargs :name 'replay-proxy))

(defvar *replay-proxy* nil)

(defvar *proxy-port* 5003)

(defun ensure-proxy ()
  "Ensure the proxy is running and return the URL to the proxy"
  (util:or-setf
   *replay-proxy*
   (let ((proxy (make-instance 'replay-proxy :port *proxy-port*)))
     (hunchentoot:start proxy)
     proxy)
   :thread-safe t)
  (format nil "http://localhost:~a" (hunchentoot:acceptor-port *replay-proxy*)))

(defmacro def-proxy-handler ((name &key uri) args &body body)
  (assert name)
  `(hunchentoot:define-easy-handler (,name :uri ,uri :acceptor-names '(replay-proxy)) ,args
     ,@body))

(defun oid-pathname (oid)
  (path:catfile (cache-dir *acceptor*) (format nil "~a.png" oid)))

(def-proxy-handler (%full-page-screenshot :uri "/full-page-screenshot") (session browser uri)
  (let* ((oid (mongoid:oid-str (mongoid:oid)))
         (driver (make-driver browser :proxy nil))
         (path (oid-pathname oid))
         (webdriver-client::*uri* uri))
    (let ((webdriver-client::*session*
            (make-instance 'webdriver-client::session
                            :id session)))
      (ensure-directories-exist path)
      (full-page-screenshot driver path)
      (setf (hunchentoot:content-type*) "application/json")
      (json:encode-json-to-string
       `((:oid . ,oid)
         (:md5 . ,(ironclad:byte-array-to-hex-string (md5:md5sum-file path))))))))

(def-proxy-handler (%download :uri "/download") (oid)
  (assert (ironclad:hex-string-to-byte-array oid))
  (hunchentoot:handle-static-file (oid-pathname oid)))

(def-proxy-handler (%status :uri "/status") ()
  "OK")
