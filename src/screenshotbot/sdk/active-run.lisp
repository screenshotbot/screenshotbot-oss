;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/active-run
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/sdk
                #:request)
  (:import-from #:screenshotbot/api/model
                #:decode-json)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/active-run)

(defun %get-active-runs (api-context &key channel)
  (multiple-value-bind (body code)
      (request api-context
               (format nil "/api/runs/active?channel=~a" channel)
               :method :get
               :decode-response nil)
    (unless (= 200 code)
      (error "Request failed with: ~a" body))
    (decode-json body '(:list dto:run))))

(defun find-active-run (api-context
                        &key channel
                          branch)
  (let ((runs (%get-active-runs api-context :channel channel)))
    (log:debug "Branches are: ~S"
               (mapcar #'dto:main-branch runs))
    (cond
      ((and (str:emptyp branch)
            (= 1 (length runs)))
       (car runs))
      ((not (str:emptyp branch))
       (loop for run in runs
             if (equal (dto:main-branch run) branch)
               return run))
      (t
       (error "Could not disambiguate branch name, pass the --branch argument")))))
