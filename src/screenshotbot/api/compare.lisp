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
                #:screenshot-name
                #:current-company
                #:can-view!)
  (:import-from #:util/store/object-id
                #:oid
                #:find-by-oid)
  (:import-from #:screenshotbot/diff-report
                #:diff-report-title
                #:diff-report-empty-p
                #:make-diff-report)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-company
                #:recorder-run)
  (:import-from #:core/installation/installation
                #:installation-domain)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:screenshotbot/report-api
                #:report-previous-run
                #:report-run
                #:report)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:util/throttler
                #:throttle!
                #:throttler)
  (:local-nicknames
   (#:dto #:screenshotbot/api/model)
   (#:diff-report #:screenshotbot/diff-report)))
(in-package :screenshotbot/api/compare)

(defvar *throttler*
  (make-instance 'throttler :tokens 1000))

(defun %api-compare-runs (run to)
  (assert-company run)
  (?. assert-company to)
  (throttle! *throttler* :key (auth:current-company))
  (let ((diff-report (make-diff-report run to)))
    (if (diff-report-empty-p diff-report)
        (make-instance 'dto:comparison
                       :samep t)
        (make-instance 'dto:comparison
                       :samep nil
                       :title (diff-report-title diff-report)
                       :changes
                       (or
                        (loop for change in (diff-report:diff-report-changes diff-report)
                              collect (screenshot-name (diff-report:after change)))
                        #())
                       :added
                       (or
                        (mapcar #'screenshot-name (diff-report:diff-report-added diff-report))
                        #())
                       :deleted
                       (or
                        (mapcar #'screenshot-name (diff-report:diff-report-deleted diff-report))
                        #())                       
                       :url (format nil "~a/runs/~a/compare/~a"
                                    (installation-domain (installation))
                                    (oid run)
                                    (?. oid to))))))

(defun assert-company (run)
  "A site-admin key could technically read every run, and a key from one
user could technically read runs from every company they are part
of. So we add an extra assertion here. We probably need to make this
part of the access check framework."
  (assert run)
  (assert (eql (current-company) (recorder-run-company run))))

(defapi (api-compare-runs :uri "/api/run/:id/compare/:to") (id to)
  (flet ((find-run (id)
           (let ((ret (find-by-oid id 'recorder-run)))
             (assert ret)
             ret)))
   (let ((run (find-run id))
         (to (find-run to)))
     (can-view! run to)
     (%api-compare-runs run to))))

(defapi (api-compare-report :uri "/api/report/:id/comparison" :wrap-success nil) (id)
  (let ((report (find-by-oid id 'report)))
    (can-view! report)
    (%api-compare-runs
     (report-run report)
     (report-previous-run report))))
