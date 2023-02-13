;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/failed-run
  (:use #:cl)
  (:import-from #:screenshotbot/api/core
                #:*dtd*
                #:defapi)
  (:import-from #:screenshotbot/model/failed-run
                #:failed-runs-for-company
                #:failed-run-commit
                #:failed-run-channel
                #:failed-run)
  (:import-from #:screenshotbot/user-api
                #:current-company))
(in-package :screenshotbot/api/failed-run)


(defclass impex-failed-run ()
  ((id :initarg :id
       :attribute "id")
   (channel :initarg :channel
            :element "channel"
            :reader failed-run-channel)
   (commit :initarg :commit
           :element "commit"
           :reader failed-run-commit))
  (:dtd-name *dtd*)
  (:element "failed-run")
  (:metaclass bknr.impex:xml-class))

(defclass impex-failed-runs ()
  ((runs :initarg :runs
         :element "failed-run"))
  (:dtd-name *dtd*)
  (:element "failed-runs")
  (:metaclass bknr.impex:xml-class))

(defun parse-body (class-name)
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (uiop:with-temporary-file (:stream s :pathname p :direction :output)
      (write-string body s)
      (finish-output s)
      (log:info "Got: ~a" (uiop:read-file-string p))
      (bknr.impex:parse-xml-file
       p
       (list (find-class class-name))))))

(defun to-impex (ret)
  (make-instance 'impex-failed-run
                 :id (bknr.datastore:store-object-id ret)
                 :channel (failed-run-channel ret)
                 :commit (failed-run-commit ret)))

(defapi (nil :uri "/api/failed-run" :method :put :type :v2) ()
  (assert (current-company))
  (let ((input (parse-body 'impex-failed-run)))
    (let ((ret
            (make-instance 'failed-run
                           :channel (failed-run-channel input)
                           :company (current-company)
                           :commit (failed-run-commit input))))
      (to-impex ret))))

(defapi (nil :uri "/api/failed-run" :method :get :type :v2) ()
  (let ((runs (failed-runs-for-company (current-company))))
    (let ((runs (loop for run in runs
                      collect (to-impex run))))
      (make-instance
       'impex-failed-runs
       :runs runs))))
