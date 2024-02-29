;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/batch
  (:use #:cl)
  (:import-from #:screenshotbot/api/core
                #:defapi)
  (:import-from #:screenshotbot/model/batch
                #:batch-commit
                #:batch-name
                #:find-or-create-batch
                #:batch)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-repo-url)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/batch)

(defapi (post-batch :uri "/api/batch" :method :post :use-yason t) ()
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (let ((dto (json-mop:json-to-clos body 'dto:batch)))
      (batch-to-dto
       (make-batch-from-dto dto (auth:current-company))))))

(defun make-batch-from-dto (dto company)
  (find-or-create-batch
   :company company
   :repo (dto:batch-repo dto)
   :commit (dto:batch-commit dto)
   :name (dto:batch-name dto)
   :pull-request-url (dto:pull-request-url dto)
   :phabricator-diff-id (dto:phabricator-diff-id dto)))

(defun batch-to-dto (batch)
  (make-instance 'dto:batch
                 :id (store-object-id batch)
                 :github-repo (recorder-run-repo-url batch)
                 :name (batch-name batch)
                 :commit (batch-commit batch)))
