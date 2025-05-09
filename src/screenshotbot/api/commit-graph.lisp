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
        #:screenshotbot/user-api)
  (:import-from #:util/events
                #:push-event
                #:with-tracing)
  (:import-from #:screenshotbot/model/commit-graph
                #:commit-graph-refs
                #:commit-graph-set-ref
                #:merge-dag-into-commit-graph)
  (:import-from #:serapeum
                #:collecting)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/commit-graph)

(defun %merge-dags (repo-url new-dag)
  (log:info "Updating commit graph for ~S and ~S" (current-company) repo-url)
  (with-tracing (:merge-dags :repo-url repo-url)
    (let* ((commit-graph (find-or-create-commit-graph
                          (current-company)
                          repo-url)))
      (merge-dag-into-commit-graph commit-graph
                                   new-dag))))


(auto-restart:with-auto-restart ()
 (defun %update-commit-graph-v2 (repo-url graph-json)
   (%merge-dags repo-url (dag:read-from-stream (make-string-input-stream graph-json)))))

(defun update-commit-graph-for-text (repo-url data)
  (%merge-dags repo-url
               (dag:read-from-stream
                (make-string-input-stream data)
                :format :text)))

(defapi (update-commit-graph :uri "/api/commit-graph" :method :post) (repo-url graph-json format
                                                                               refs)
  ;; do nothing with this for the moment
  (cond
    ((string-equal "text" format)
     (update-commit-graph-for-text repo-url
                                   (hunchentoot:raw-post-data :want-stream nil
                                                              :force-text t)))
    (t
     (%update-commit-graph-v2 repo-url graph-json)
     (when (str:non-empty-string-p refs)
       (log:info "Using new commit graph api")
       (push-event :commit-graph-api-with-refs :company (format nil "~a" (auth:current-company)))
       (update-refs :repo-url repo-url
                    :refs refs))
     "OK")))

(defapi (get-refs :uri "/api/commit-graph/refs" :method :get :wrap-success nil) (repo-url)
  (let ((commit-graph (find-or-create-commit-graph (current-company) repo-url)))
    (or
     (collecting
       (fset:do-map (key value (commit-graph-refs commit-graph))
         (collect (make-instance 'dto:git-ref
                                  :name key
                                  :sha value))))
     #())))


(defapi (update-refs :uri "/api/commit-graph/refs" :method :post) (repo-url refs)
  "This isn't intended to be used in prod at the moment, it's mostly a convenience for testing"
  (let ((commit-graph (find-or-create-commit-graph (current-company) repo-url))
        (refs (dto:decode-json refs '(:list dto:git-ref))))
    (loop for ref in refs
          do (commit-graph-set-ref commit-graph (dto:git-ref-name ref)
                                   (dto:git-ref-sha ref)))))
