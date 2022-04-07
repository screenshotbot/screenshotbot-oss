;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/testing
  (:use #:cl)
  (:import-from #:nibble
                #:nibble-plugin)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:with-fake-request
   #:in-test-p
   #:screenshot-static-page))
(in-package :util/testing)

(defvar *in-test-p* nil)

(defun in-test-p ()
  *in-test-p*)

(defclass custom-request (hunchentoot:request)
  ((additional-post-params :initform nil)))

(defmethod (setf hunchentoot:post-parameter) (val (request custom-request) key)
  (with-slots (additional-post-params) request
    (setf (a:assoc-value additional-post-params key :test 'equal)
          val)))

(defmethod hunchentoot:post-parameters ((request custom-request))
  (with-slots (additional-post-params) request
   (append
    additional-post-params
    (call-next-method))))

(defclass test-acceptor (hex:base-acceptor
                         hex:acceptor-with-plugins)
  ()
  (:default-initargs :acceptor-plugins (list (make-instance 'nibble-plugin))))

(defmacro with-fake-request ((&key  (acceptor '(quote test-acceptor)) (host "localhost")
                                (script-name "/")) &body body)
 `(let* ((hunchentoot::*hunchentoot-stream*)
         (hunchentoot:*catch-errors-p* nil)
         (hunchentoot:*acceptor* (make-instance ,acceptor
                                                :db-config '(:mysqladmin nil)))
         (hunchentoot:*reply* (make-instance 'hunchentoot:reply))
         (hunchentoot:*request* (make-instance 'custom-request
                                               :acceptor hunchentoot:*acceptor*
                                               :headers-in (list (cons :host ,host))
                                               :content-stream nil
                                               :uri ,script-name
                                               :method :get
                                               :server-protocol :https
                                               :remote-addr "127.0.0.1")))
    ,@body))

(defun screenshot-static-page (project name content)
  (let ((output (asdf:system-relative-pathname project "static-web-output/")))
    (let ((output-file (path:catfile output (format nil "~a/index.html" name))))
      (ensure-directories-exist output-file)
      (with-open-file (file output-file
                            :direction :output
                            :if-exists :supersede)
        (write-string content file)
        (fiveam:pass "Screenshot written")))))
