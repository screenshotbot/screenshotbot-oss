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
                #:direct-selenium-url
                #:relay-session-request
                #:request-session-and-respond
                #:hub)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:http-proxy/server
                #:allowedp
                #:http-proxy)
  (:import-from #:util/threading
                #:ignore-and-log-errors)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:*proxy-port*
   #:ensure-proxy
   #:selenium-host
   #:selenium-port))
(in-package :screenshotbot/replay/proxy)

(defvar *screenshot-proxy-cache*
  "~/screenshot-proxy-cache/")

(defvar *lock* (bt:make-lock))

(defclass custom-http-proxy (http-proxy)
  ((allowed-hosts :initform nil
                  :accessor allowed-hosts)))

(defmethod allowedp ((self custom-http-proxy)
                     host)
  (let ((host (car (str:split ":" host))))
    (bt:with-lock-held (*lock*)
     (str:s-member (allowed-hosts self) host))))

(defclass replay-proxy (hunchentoot:easy-acceptor)
  ((cache-dir :initarg :cache-dir
              :reader cache-dir
              :initform (pathname *screenshot-proxy-cache*))
   (hub :initarg :hub
        :initform (hub)
        :reader replay-proxy-hub)
   (http-proxy :initarg :http-proxy
               :initform (make-instance 'custom-http-proxy
                                        :port 3129)
               :reader http-proxy))
  (:default-initargs :name 'replay-proxy)
  (:documentation "Even though this is called 'replay' it refers to a Selenium proxy,
 with some added functinality for screenshots."))

(defmethod hunchentoot:start :before ((self replay-proxy))
  (hunchentoot:start (http-proxy self)))

(defmethod hunchentoot:stop :after ((self replay-proxy) &key &allow-other-keys)
  (hunchentoot:stop (http-proxy self)))

(defun linode? ()
  #-screenshotbot-oss
  (or
   (equal "localhost" (uiop:hostname))
   (equal "prod1.screenshotbot.io" (uiop:hostname))))

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
     (format nil "http://~a:~d"
             (selenium-host selenium-service)
             (selenium-port selenium-service)))
    #+screenshotbot-oss
    (t
     (ensure-local-proxy))
    (t
     ;; This is the docker host. The proxy will run on the host
     ;; container since we need it to create docker containers.
     "http://172.17.0.1:5004")))

(def-easy-macro with-error-handling (&fn fn)
  (handler-bind ((error (lambda (e)
                          (setf (hunchentoot:return-code*) 500)
                          (return-from call-with-error-handling
                            (json:encode-json-to-string
                             `(("value" . (("error" . ,(%print-condition e))
                                           ("message" . "Error forwarding request")
                                           #+lispworks
                                           ("stacktrace" .
                                                         ,(with-output-to-string (out)
                                                            (dbg:output-backtrace :debug :stream out)))))))))))
    (funcall fn)))

(defun %handler-wrap (fn)
  (with-error-handling ()
    (funcall fn)))

(defmacro def-proxy-handler ((name &key uri method) args &body body)
  `(hex:better-easy-handler (,name :uri ,uri :acceptor-names '(replay-proxy) :method ,method) ,args
     (%handler-wrap
      (lambda ()
        ,@body))))


(defun oid-pathname (oid)
  (path:catfile (cache-dir *acceptor*) (format nil "~a.png" oid)))

(defun %print-condition (e)
  (let ((*print-escape* nil))
    (format nil "~a" e)))


(def-proxy-handler (%full-page-screenshot :uri "/full-page-screenshot") (session browser)
  (let* ((oid (mongoid:oid-str (mongoid:oid)))
         (driver (make-driver browser :proxy nil))
         (path (oid-pathname oid))
         (webdriver-client::*uri* (direct-selenium-url
                                   (replay-proxy-hub hunchentoot:*acceptor*)
                                   session)))
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

(defun add-remote-to-proxy-acl ()
  ;; Note that we're using remote-addr, and not real-remote-addr. If
  ;; the incoming request was being forwarded from somewhere, then you
  ;; need to specify that directly.
  (let ((host (hunchentoot:remote-addr hunchentoot:*request*)))
    (bt:with-lock-held (*lock*)
      (pushnew host
               (allowed-hosts (http-proxy hunchentoot:*acceptor*))))))

(def-proxy-handler (nil :uri "/wd/hub/session" :method :post) ()
  (add-remote-to-proxy-acl)
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

(defun clean-old-screenshots ()
  (let ((cut-off (- (get-universal-time)
                    (* 10 60))))
   (loop for file in  (fad:list-directory *screenshot-proxy-cache*)
         for write-date = (file-write-date file)
         if (< write-date cut-off)
           do
              (log:info "Deleting file ~a" file)
              (delete-file file))))

;; We can't use def-cron here because there's no datastore
(cl-cron:make-cron-job
 (lambda ()
   (ignore-and-log-errors ()
     (clean-old-screenshots)))
 :hash-key 'clean-old-screenshots
 :step-min 5)
