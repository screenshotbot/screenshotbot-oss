;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/dev/verify-against-ci
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/dev/record-verify
                #:compare-and-log
                #:%make-run-and-get-id
                #:default-options)
  (:import-from #:clingon.options
                #:make-option)
  (:import-from #:screenshotbot/sdk/clingon-api-context
                #:with-clingon-api-context)
  (:import-from #:clingon.command
                #:getopt)
  (:import-from #:screenshotbot/sdk/sdk
                #:request)
  (:import-from #:screenshotbot/api/model
                #:decode-json)
  (:import-from #:util/misc
                #:not-null!)
  (:import-from #:parse-float
                #:parse-float)
  (:local-nicknames (#:run-context #:screenshotbot/sdk/run-context)
                    (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/dev/verify-against-ci)

(defun find-base-run (api-ctx channel commit)
  (let ((res (request api-ctx
                      (format nil "/api/find-base-run?channel=~a&commit=~a"
                              channel commit)
                      :method :get
                      :decode-response nil)))
    (decode-json
     res
     '(or null dto:run))))

(defun %verify-against-ci (api-ctx cmd)
  (let* ((channel (getopt cmd :channel))
         (commit (getopt cmd :base-commit-hash))
         (base-run (find-base-run api-ctx channel commit))
         (run (%make-run-and-get-id
               api-ctx
               :channel channel
               :directory (getopt cmd :directory)
               :recursivep (getopt cmd :recursivep)
               :file-types (str:split "," (getopt cmd :image-file-types)))))
    (unless base-run
      (error "Could not find run for commit: ~a" commit))
    (compare-and-log api-ctx run base-run)))

(defun %options ()
  (list*
   (make-option
    :string
    :long-name "threshold"
    :initial-value "0"
    :description "The threshold to use to compare"
    :key :threshold)
   (make-option
    :string
    :long-name "base-commit"
    :description "The base commit-SHA to compare against"
    :key :base-commit-hash
    :required t)
   (default-options)))

(defun parse-threshold (str)
  (flet ((threshold-error ()
           (error "Threshold must be a floating point number between 0 and 1: got ~a" str)))
    (let ((res (handler-case
                   (parse-float str)
                 (alexandria:simple-parse-error (e)
                   (threshold-error)))))
      (unless (<= 0 res 1)
        (threshold-error))
      res)))


(defun verify-against-ci/command ()
  (clingon:make-command
   :name "verify-against-ci"
   :description "Verify your local screenshots against a CI version"
   :options (%options)
   :handler (lambda (cmd)
              (with-clingon-api-context (api-ctx cmd)
                (%verify-against-ci
                 api-ctx
                 cmd)))))


