;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/compare
  (:use #:cl)
  (:import-from #:screenshotbot/api/core
                #:defapi)
  (:import-from #:screenshotbot/user-api
                #:can-view!)
  (:import-from #:util/store/object-id
                #:oid
                #:find-by-oid)
  (:import-from #:screenshotbot/diff-report
                #:diff-report-title
                #:diff-report-empty-p
                #:make-diff-report)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run)
  (:import-from #:core/installation/installation
                #:installation-domain)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:local-nicknames
   (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/compare)

(defun %api-compare-runs (run to)
  (let ((diff-report (make-diff-report run to)))
    (if (diff-report-empty-p diff-report)
        (make-instance 'dto:comparison
                       :samep t)
        (make-instance 'dto:comparison
                       :samep nil
                       :title (diff-report-title diff-report)
                       :url (format nil "~a/runs/~a/compare/~a"
                                    (installation-domain (installation))
                                    (oid run)
                                    (oid to))))))

(defapi (api-compare-runs :uri "/api/run/:id/compare/:to") (id to)
  (flet ((find-run (id)
           (let ((ret (find-by-oid id 'recorder-run)))
             (assert ret)
             ret)))
   (let ((run (find-run id))
         (to (find-run to)))
     (can-view! run to)
     (%api-compare-runs run to))))
