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


(defun get-run (api-context oid)
  (multiple-value-bind (body code)
      (request api-context
               (format nil "/api/run/~a" oid)
               :method :get
               :decode-response nil)
    ;; TODO: sync this with ENSURE-API-SUCCESS.
    (unless (eql code 200)
      (error "Could not fetch run: ~a, ~a" code body))
    (json-mop:json-to-clos
     body
     'dto:run)))

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

(defun trim-/ (name)
  (cl-ppcre:regex-replace "^/*" name ""))

(defun %save-run (run &key output)
  (loop for screenshot in (dto:run-screenshots run)
        do
           (let ((screenshot-name (trim-/ (dto:screenshot-name screenshot))))
             (unless (safe-name-p screenshot-name)
               (error 'unsafe-screenshot-name))
             (let ((output
                     (format nil "~a.png"
                             (path:catfile output screenshot-name))))
               (log:info "Saving: ~a" output)
               (let ((url (dto:screenshot-url screenshot)))
                 (log:debug "URL is: ~a" url)
                 (download-url
                  url
                  output))))))




