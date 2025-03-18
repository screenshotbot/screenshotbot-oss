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
                #:batch-item-report
                #:batch-items
                #:batch-commit
                #:batch-name
                #:find-or-create-batch
                #:batch)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-repo-url)
  (:import-from #:util/store/object-id
                #:find-by-oid)
  (:import-from #:screenshotbot/model/report
                #:report-to-dto)
  (:import-from #:screenshotbot/api/doc
                #:def-api-doc)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/batch)

(define-condition validation-error (error)
  ((message :initarg :message))
  (:report (lambda (e out)
            (format out (slot-value e 'message)))))

(defmethod validate-dto ((batch dto:batch))
  (flet ((verify (test message &rest args)
           (unless test
             (error 'validation-error
                    :message
                    (apply #'format nil message args)))))
    (verify (< (length (dto:batch-name batch)) 256)
            "Batch name too long")
    (verify (eql (length (dto:batch-commit batch)) 40)
            "Commit should be exactly 40 characters long")
    (verify (< (length (dto:batch-repo batch)) 256)
            "Batch repo too long")
    (verify (< (length (dto:pull-request-url batch)) 256)
            "Pull request URL too long")
    (verify (or (null (dto:phabricator-diff-id batch))
                (numberp (dto:phabricator-diff-id batch)))
            "Phabricator diff id must be a number")))

(defapi (post-batch :uri "/api/batch" :method :post :use-yason t) ()
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (let ((dto (json-mop:json-to-clos body 'dto:batch)))
      (batch-to-dto
       (make-batch-from-dto dto (auth:current-company))))))

(defun make-batch-from-dto (dto company)
  (validate-dto dto)
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

(defapi (get-reports :uri "/api/batch/:oid/reports" :use-yason t :wrap-success nil) (oid)
  (let ((batch (find-by-oid oid)))
    (coerce
     (loop for item in (fset:convert 'list (batch-items batch))
           for report = (batch-item-report item)
           if report
             collect
             (progn
               (auth:can-view! report)
               (report-to-dto report)))
     'vector)))


