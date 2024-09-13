;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/fetch-run
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/api-context
                #:api-context)
  (:import-from #:screenshotbot/sdk/sdk
                #:request)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
  (:import-from #:util/request
                #:engine
                #:http-request)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/fetch-run)

(defvar *download-engine*
  (make-instance 'engine))

(defclass fetch-run-response ()
  ((run :json-type dto:run
        :json-key "response"
        :reader run))
  (:metaclass ext-json-serializable-class))

(defun get-run (api-context oid)
  (let* ((body (request api-context
                        (format nil "/api/run/~a" oid)
                        :method :get
                        :decode-response nil))
         (fetch-run-response (json-mop:json-to-clos
               body
               'fetch-run-response)))
    (run fetch-run-response)))

(define-condition unsafe-screenshot-name (error)
  ())

(defun safe-name-p (screenshot-name)
  (let ((path (pathname screenshot-name)))
    (and
     (not (eql :absolute (car (pathname-directory path))))
     (loop for directory in (pathname-directory path)
           if (or (equal ".." directory) ;; it's actually just :up, but keeping it here in case
                  (equal :up directory))
             return nil
           finally
              (return t)))))

(defun save-run (api-context oid &key output)
  (let ((run (get-run api-context oid)))
    (%save-run run :output output)))

(defun download-url (url file)
  (with-open-file (output (ensure-directories-exist file) :direction :output
                               :element-type '(unsigned-byte 8))
    (with-open-stream (input (http-request
                         url
                         :want-stream t
                         :engine *download-engine*))
      (uiop:copy-stream-to-stream
       input
       output
       :element-type '(unsigned-byte 8)))))

(defun %save-run (run &key output)
  (loop for screenshot in (dto:run-screenshots run)
        do
           (unless (safe-name-p (dto:screenshot-name screenshot))
             (error 'unsafe-screenshot-name))
           (log:info "Saving: ~a" (dto:screenshot-url screenshot))
           (let ((output
                   (make-pathname
                    :type "png"
                    :defaults (path:catfile output (dto:screenshot-name screenshot)))))
             (download-url
              (dto:screenshot-url screenshot)
              output))))


