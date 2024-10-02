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
  (:import-from #:util/threading
                #:make-thread
                #:max-pool)
  (:import-from #:util/lists
                #:make-batches
                #:with-batches)
  (:import-from #:screenshotbot/sdk/active-run
                #:find-active-run)
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
    (log:debug "Got /api/run/... result: ~s" body)
    ;; TODO: sync this with ENSURE-API-SUCCESS.
    (unless (eql code 200)
      (error "Could not fetch run: ~a, ~a" code body))
    (json-mop:json-to-clos
     body
     'dto:run)))

(defvar *lock* (bt:make-lock))

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

(defun save-run (api-context oid &key output channel branch)
  (cond
    (oid
     (let ((run (get-run api-context oid)))
       (%save-run run :output output)))
    (t
     (log:debug "Looking up active run for ~a, ~a" channel branch)
     (let ((run (find-active-run api-context :channel channel :branch branch)))
       (unless run
         (error "Could not find active run for that combination of channel and branch"))
       (log:info "Figured out the run as ~a" (dto:run-id run))
       (save-run api-context (dto:run-id run) :output output)))))

#+lispworks
(defvar *semaphore* (mp:make-semaphore :count 2)
  "We don't want all three threads being blocked on latency at the same time.")

(defun download-url (url file &key engine)
  (with-open-file (output (bt:with-lock-held (*lock*)
                            (ensure-directories-exist file))
                          :direction :output
                          :element-type '(unsigned-byte 8))
    #+lispworks
    (mp:semaphore-acquire *semaphore*)

    (with-open-stream (input (unwind-protect
                                  (http-request
                                   url
                                   :want-stream t
                                   :engine engine)
                               #+lispworks
                               (mp:semaphore-release *semaphore*)))
      (uiop:copy-stream-to-stream
       input
       output
       :element-type '(unsigned-byte 8)))))

(defun trim-/ (name)
  (cl-ppcre:regex-replace "^/*" name ""))

(defun download-batch-of-screenshots (screenshots &key output engine)
  (loop for screenshot in screenshots
        do
           (let ((screenshot-name (trim-/ (dto:screenshot-name screenshot))))
             (unless (safe-name-p screenshot-name)
               (error 'unsafe-screenshot-name))
             (let ((output
                     (format nil "~a.png"
                             (path:catfile output screenshot-name))))
               (log:info "Saving: ~a" output)
               (let ((url (dto:screenshot-url screenshot)))
                 (unless (str:emptyp (uiop:getenv "SCREENSHOTBOT_DEBUG_IGNORE_CDN"))
                   (log:warn "Ignoring CDN!")
                   (setf url (str:replace-all "cdn.screenshotbot.io" "screenshotbot.io" url)))
                 (log:debug "URL is: ~a" url)
                 (download-url
                  url
                  output
                  :engine engine))))))

(defun %save-run (run &key output)
  (let ((screenshots (dto:run-screenshots run)))
    (let* ((batches (make-batches screenshots :batch-size (ceiling (length screenshots) 3)))
           (engine *download-engine*)
           (finish-count 0)
           (e nil)
           (threads (loop for batch in batches
                          collect
                          (let ((batch batch))
                            (make-thread
                             (lambda ()
                               (handler-bind ((error (lambda (%e)
                                                       (setf e %e))))
                                 (download-batch-of-screenshots batch :output output
                                                                      :engine engine))
                               (bt:with-lock-held (*lock*)
                                 (incf finish-count))))))))
     (mapc #'bt:join-thread threads)
      (unless (eql finish-count (length threads))
        (cond
          (e
           (error e))
          (t
           (error "Some threads failed to complete and we don't know why")))))))




