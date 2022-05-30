(defpackage :util/request
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:http-request))
(in-package :util/request)

(defun http-request (url &rest args &key (verify #-mswindows t #+mswindows nil) &allow-other-keys )
  (apply #'drakma:http-request url
           :verify verify
           args))
