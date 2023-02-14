;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/failed-run
  (:use #:cl)
  (:import-from #:screenshotbot/api/core
                #:api-error
                #:*dtd*
                #:defapi)
  (:import-from #:screenshotbot/model/failed-run
                #:failed-runs-for-company
                #:failed-run-commit
                #:failed-run-channel
                #:failed-run)
  (:import-from #:screenshotbot/user-api
                #:current-company)
  (:import-from #:json-mop
                #:json-serializable-class)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/failed-run)

(defun parse-body (class-name)
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (json-mop:json-to-clos body class-name)))

(defun to-dto (ret)
  (make-instance 'dto:failed-run
                 :id (bknr.datastore:store-object-id ret)
                 :channel (failed-run-channel ret)
                 :commit (failed-run-commit ret)))

(defapi (%put-failed-run :uri "/api/failed-run" :method :put) ()
  (assert (current-company))
  (let ((input (parse-body 'dto:failed-run)))
    (let ((ret
            (make-instance 'failed-run
                           :channel (dto:failed-run-channel input)
                           :company (current-company)
                           :commit (dto:failed-run-commit input))))
      (to-dto ret))))

(defapi (%list-failed-runs :uri "/api/failed-run" :method :get) ()
  (let ((runs (failed-runs-for-company (current-company))))
    (loop for run in runs
          collect (to-dto run))))
