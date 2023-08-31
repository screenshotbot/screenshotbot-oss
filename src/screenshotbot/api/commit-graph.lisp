;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/api/commit-graph
  (:use #:cl
        #:alexandria
        #:screenshotbot/api/core
        #:screenshotbot/model/commit-graph
        #:screenshotbot/user-api))
(in-package :screenshotbot/api/commit-graph)

(defun %merge-dags (repo-url new-dag)
  (log:info "Updating commit graph for ~S and ~S" (current-company) repo-url)
  (let* ((commit-graph (find-or-create-commit-graph
                        (current-company)
                        repo-url)))
    (bt:with-recursive-lock-held ((lock commit-graph))
      (let ((dag (commit-graph-dag commit-graph)))
         (dag:merge-dag dag new-dag)
         (setf (commit-graph-dag commit-graph)
               dag)))))

(auto-restart:with-auto-restart ()
 (defun %update-commit-graph-v2 (repo-url graph-json)
   (%merge-dags repo-url (dag:read-from-stream (make-string-input-stream graph-json)))))

(defun update-commit-graph-for-text (repo-url data)
  (%merge-dags repo-url
               (dag:read-from-stream
                (make-string-input-stream data)
                :format :text)))

(defapi (update-commit-graph :uri "/api/commit-graph" :method :post) (repo-url graph-json format)
  ;; do nothing with this for the moment
  (cond
    ((string-equal "text" format)
     (update-commit-graph-for-text repo-url
                                   (hunchentoot:raw-post-data :want-stream nil
                                                              :force-text t)))
    (t
     (%update-commit-graph-v2 repo-url graph-json))))
