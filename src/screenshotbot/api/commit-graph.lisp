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

(defapi (update-commit-graph :uri "/api/commit-graph" :method :post) (repo-url branch graph-json)
  ;; do nothing with this for the moment
  (%update-commit-graph repo-url branch graph-json))

(defun %update-commit-graph (repo-url branch graph-json)
  (log:info "Updating commit graph for ~S and ~S" (current-company) repo-url)
  (restart-case
      (let* ((commit-graph (find-or-create-commit-graph
                            (current-company)
                            repo-url)))
        (bt:with-recursive-lock-held ((lock commit-graph))
          (let ((dag (commit-graph-dag commit-graph))
                (new-dag (dag:read-from-stream (make-string-input-stream graph-json))))
            (dag:merge-dag dag new-dag)
            (setf (commit-graph-dag commit-graph)
                  dag))))
    (retry-update-commit-graph ()
      (%update-commit-graph repo-url
                            branch
                            graph-json))))
