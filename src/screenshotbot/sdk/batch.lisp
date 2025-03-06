;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/batch
  (:use #:cl)
  (:import-from #:clingon.options
                #:make-option)
  (:import-from #:clingon.command
                #:getopt)
  (:import-from #:screenshotbot/sdk/clingon-api-context
                #:with-clingon-api-context)
  (:import-from #:screenshotbot/sdk/api-context
                #:api-context)
  (:import-from #:screenshotbot/api/model
                #:decode-json)
  (:local-nicknames (#:sdk #:screenshotbot/sdk/sdk)
                    (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/batch)

(defun list-reports (api-context &key batch-id)
  (when (str:emptyp batch-id)
    (error "Please provide a --batch-id"))
  (multiple-value-bind (result code)
      (sdk:request api-context
                   (format nil "/api/batch/~a/reports" batch-id)
                   :decode-response nil
                   :method :get)
    (unless (eql 200 code)
      (error "API request failed: ~a" result))
    (decode-json
     result
     `(:list dto:report))))

(defun print-reports (api-context &key batch-id)
  (loop for report in (list-reports api-context :batch-id batch-id)
        do (format t "~a~%" (dto:report-id report))))

(defun reports/command ()
  (clingon:make-command
   :name "reports"
   :description "Show all the reports that are part of the given batch"
   :handler (lambda (cmd)
              (with-clingon-api-context (api-context cmd)
               (print-reports api-context :batch-id (getopt cmd :batch-id))))
   :options (list
             (make-option
              :string
              :long-name "batch-id"
              :key :batch-id
              :description "The Batch ID. Typically, if the URL that is linked to it will be /batch/<id>."))))

(defun accept-report (api-context report)
  (log:info "Accepting ~a" (dto:report-id report))
  (multiple-value-bind (result code)
      (sdk:request api-context
                   (format nil "/api/report/~a/review/accept" (dto:report-id report))
                   :method :post
                   :decode-response nil)
    (unless (eql 200 code)
      (error "Could not accept report ~a, ~a" (dto:report-id report)
             result))))

(defun %accept-all (api-context &key batch-id)
  (loop for report in (list-reports api-context :batch-id batch-id)
        if (not (equal "accepted" (dto:report-acceptable-state report)))
          do
             (accept-report api-context report)
        else
          do
        (log:info "Report ~a is already accepted" (dto:report-id report))))

(defun accept-all/command ()
  (clingon:make-command
   :name "accept-all"
   :description "Accept all reports under this batch"
   :handler (lambda (cmd)
              (with-clingon-api-context (api-context cmd)
                (%accept-all api-context :batch-id (getopt cmd :batch-id))))
   :options (list
             (make-option
              :string
              :long-name "batch-id"
              :key :batch-id
              :description "The Batch ID. Typically, if the URL that is linked to it will be /batch/<id>."))))

(defun batch/command ()
  (clingon:make-command
   :name "batch"
   :description "Commands operating on Batch objects"
   :sub-commands (list
                  (reports/command)
                  (accept-all/command))))




