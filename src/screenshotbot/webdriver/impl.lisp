;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/webdriver/impl
    (:use #:cl
          #:alexandria)
  (:import-from #:webdriver-client
                #:ignore-session-deletion
                #:window-resize
                #:screenshot
                #:with-session)
  (:export   #:with-webdriver
             #:take-screenshot
             #:chrome
             #:firefox
             #:call-with-webdriver))


(defun make-dict (alist)
  (let ((ret (make-hash-table)))
    (loop for (x . y) in alist do
      (setf (gethash x ret) y))
    ret))


(defclass driver ()
  ((proxy :initarg :proxy
          :reader driver-proxy)))

(defclass chrome (driver)
  ())

(defclass firefox (driver)
  ())

(defclass safari (driver)
  ())

(defmethod extra-driver-options (driver &key &allow-other-keys)
  )

#+nil
(defun proxy ()
  "squid:3128")

(defmethod extra-driver-options ((driver firefox) &key &allow-other-keys)
  (make-default-proxy-settings driver))

(defun make-default-proxy-settings (driver)
  (let ((proxy (format nil "~a" (driver-proxy driver))))
    `(("proxy" . ,(make-dict
                   `(("proxyType" . "manual")
                     ("httpProxy" . ,proxy)
                     ("sslProxy" . ,proxy)))))))

(defmethod extra-driver-options ((driver safari) &key &allow-other-keys)
  ;; Safari doesn't support specifying proxy in capabilities. Instead
  ;; you need to physically set the proxy on the machine.
  #+nil
  (make-default-proxy-settings driver))

(defmethod extra-driver-options ((driver chrome)
                                 &key mobile-emulation
                                 &allow-other-keys)
  (declare (ignore dimensions))
  (let ((options `(("args" . ("--no-sandbox"
                              "--disable-gpu"
                              "--hide-scrollbars"
                              ,(format nil "--proxy-server=~a" (driver-proxy driver)))))))
    (when mobile-emulation
      (push `("mobileEmulation" . ,(make-dict `(("deviceName" . ,mobile-emulation))))
            options))
    `(("goog:chromeOptions" . ,(make-dict options)))))

(defun make-driver (browser &rest args &key (proxy (error "must specify :proxy")))
  (cond
    ((symbolp browser)
     (apply #'make-instance browser args))
    (t
     (apply #'make-instance
            (find browser '(chrome firefox safari) :test #'string-equal)
            args))))

(defun call-with-webdriver (fn &rest args
                            &key browser
                              mobile-emulation
                              (proxy (error "must specify proxy"))
                              dimensions)
  (log:info "got browser: ~s" browser)
  (restart-case
      (let ((driver (make-driver browser :proxy proxy)))
        (handler-bind
            ((error (lambda (e)
                      (declare (ignore e))
                      (alexandria:when-let ((restart (find-restart 'ignore-session-deletion)))
                        (log:warn "Ignore error when deleting session")
                        (invoke-restart restart)))))
         (with-session `(:always-match (("browserName" . ,(string-downcase browser))
                                        ,@ (extra-driver-options driver
                                                                 :mobile-emulation mobile-emulation
                                                                 :dimension dimensions)))
           (when dimensions
             (window-resize :width (car dimensions)
                            :height (cdr dimensions)))
           (funcall fn driver))))
    (retry-webdriver-execution ()
      (apply #'call-with-webdriver fn args))))

(defmacro with-webdriver ((driver &rest args) &body body)
  `(call-with-webdriver (lambda (,driver)
                          ,@body)
                        ,@args))
