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
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/fetch-run)

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

(defun safe-name-p (screnshot-name)
  (values))

(defun save-run (api-context oid &key output)
  (let ((run (get-run api-context oid)))
    (loop for screenshot in (dto:run-screenshots run)
          do
             (assert (safe-name-p (dto:screenshot-name screenshot)))
             (log:info "Saving: ~a" (dto:screenshot-url screenshot)))))


