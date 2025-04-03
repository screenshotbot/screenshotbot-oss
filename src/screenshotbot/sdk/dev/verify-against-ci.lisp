;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/dev/verify-against-ci
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/dev/record-verify
                #:%make-run-and-get-id
                #:default-options)
  (:import-from #:clingon.options
                #:make-option)
  (:import-from #:screenshotbot/sdk/clingon-api-context
                #:with-clingon-api-context)
  (:import-from #:clingon.command
                #:getopt))
(in-package :screenshotbot/sdk/dev/verify-against-ci)

(defun %verify-against-ci (api-ctx cmd)
  (let ((run (%make-run-and-get-id
              api-ctx
              :channel (getopt cmd :channel)
              :directory (getopt cmd :directory)
              :recursivep (getopt cmd :recursivep)
              :file-types (str:split "," (getopt cmd :image-file-types)))))
   (error "Got run: ~a" run)))

(defun verify-against-ci/command ()
  (clingon:make-command
   :name "verify-against-ci"
   :description "Verify your local screenshots against a CI version"
   :options (list*
             (make-option
              :string
              :long-name "threshold"
              :initial-value "0"
              :description "The threshold to use to compare"
              :key :threshold)
             (default-options))
   :handler (lambda (cmd)
              (with-clingon-api-context (api-ctx cmd)
                (%verify-against-ci
                 api-ctx
                 cmd)))))


