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
  (:import-from #:util/digests
                #:md5-file)
  (:import-from #:screenshotbot/hub/server
                #:relay-session-request
                #:request-session-and-respond
                #:hub)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:*proxy-port*
   #:ensure-proxy))
(in-package :screenshotbot/replay/proxy)

(defclass replay-proxy (hunchentoot:easy-acceptor)
  ((cache-dir :initarg :cache-dir
              :reader cache-dir
              :initform (pathname "~/screenshot-proxy-cache/"))
   (hub :initarg :hub
        :initform (hub)
        :reader replay-proxy-hub))
  (:default-initargs :name 'replay-proxy))

(defun linode? ()
  #-screenshotbot-oss
  (equal "localhost" (uiop:hostname)))

(defvar *replay-proxy* nil)

(defvar *proxy-port* 5003)

(defun ensure-local-proxy ()
  (util/misc:or-setf
   *replay-proxy*
   (let ((proxy (make-instance 'replay-proxy :port *proxy-port*)))
     (hunchentoot:start proxy)
     proxy)
   :thread-safe t)
  (format nil "http://localhost:~a" (hunchentoot:acceptor-port *replay-proxy*)))

;; If you change this behaves, make sure it looks reasonable in launch-proxy.lisp
(defun ensure-proxy (selenium-service)
  "Ensure the proxy is running and return the URL to the proxy"
  (declare (ignore selenium-service))
  (cond
    ((linode?)
     ;; todo: error handling: if this endpoint goes down then just a local
     ;; proxy.
     "http://10.9.8.2:5004")
    #+screenshotbot-oss
    (t
     (ensure-local-proxy))
    (t
     ;; This is the docker host. The proxy will run on the host
     ;; container since we need it to create docker containers.
     "http://172.17.0.1:5004")))

(defun %handler-wrap (fn)
  (funcall fn))

(defmacro def-proxy-handler ((name &key uri method) args &body body)
  `(hex:better-easy-handler (,name :uri ,uri :acceptor-names '(replay-proxy) :method ,method) ,args
     (%handler-wrap
      (lambda ()
        ,@body))))


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
         (:md5 . ,(ironclad:byte-array-to-hex-string (md5-file path))))))))

(def-proxy-handler (nil :uri "/wd/hub") ()
  (error "Unimpl"))

(def-proxy-handler (nil :uri "/wd/hub/session" :method :post) ()
  (let ((content (hunchentoot:raw-post-data
                  :force-text t)))
    (let ((hub (replay-proxy-hub hunchentoot:*acceptor*)))
      (request-session-and-respond
       hub
       content))))

(def-proxy-handler (nil :uri (lambda (request)
                               (let ((script-name (hunchentoot:script-name request)))
                                 (let ((prefix "/wd/hub/session/"))
                                  (and
                                   (str:starts-with-p prefix script-name)
                                   (> (length script-name) (length prefix)))))))
                   ()
  (let ((hub (replay-proxy-hub hunchentoot:*acceptor*)))
    (relay-session-request hub
                           :method (hunchentoot:request-method*)
                           :content (hunchentoot:raw-post-data :force-text t)
                           :content-type (hunchentoot:content-type*)
                           :script-name (hunchentoot:script-name*))))

(def-proxy-handler (%download :uri "/download") (oid)
  (assert (ironclad:hex-string-to-byte-array oid))
  (hunchentoot:handle-static-file (oid-pathname oid)))

(def-proxy-handler (%status :uri "/status") ()
  "OK")
