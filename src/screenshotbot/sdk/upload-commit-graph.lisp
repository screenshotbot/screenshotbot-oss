;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/upload-commit-graph
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/clingon-api-context
                #:with-clingon-api-context)
  (:import-from #:screenshotbot/sdk/sdk
                #:update-commit-graph)
  (:import-from #:clingon.command
                #:getopt)
  (:import-from #:screenshotbot/sdk/run-context
                #:run-context)
  (:import-from #:clingon.options
                #:make-option)
  (:import-from #:screenshotbot/sdk/env
                #:make-env-reader
                #:build-url)
  (:import-from #:screenshotbot/sdk/commit-graph
                #:commit-graph-updater)
  (:import-from #:screenshotbot/sdk/request
                #:request)
  (:import-from #:screenshotbot/api/model
                #:build-info)
  (:import-from #:alexandria
                #:when-let)
  (:local-nicknames (#:run-context #:screenshotbot/sdk/run-context)))
(in-package :screenshotbot/sdk/upload-commit-graph)


(defclass commit-graph-run-context (run-context:run-context
                                    run-context:env-reader-run-context)
  ()
  (:default-initargs
   :env (make-env-reader)))

(defun upload-commit-graph/command ()
  (clingon:make-command
   :name "upload-commit-graph"
   :handler #'upload-commit-graph/handler
   :options (list
             (make-option
              :string
              :long-name "repo-url"
              :initial-value nil
              :description "The URL of the repository (e.g. 'https://github.com/foo/bar')"
              :key :repo-url)
             (make-option
              :string
              :long-name "main-branch"
              :initial-value nil
              :description "The main branch to compare this run with. e.g. `main`, `master`, or `trunk`. The default is to first try `main`, and then `master` and pick the first such that origin/<name> exists."
              :key :main-branch))
   :description "Uploads the current commit graph. Useful if you need to upload multiple channels in the same CI job, and want to avoid uploading commit graph multiple times."))

(defun push-build-info-if-xcode-cloud (api-context run-context)
  "Push build-info if running in Xcode Cloud environment"
  (when (uiop:getenv "CI_XCODE_CLOUD")
    (when-let ((build-url (run-context:build-url run-context))
               (repo-url (run-context:repo-url run-context)))
      (log:info "Detected Xcode Cloud, pushing build-info with build-url: ~a and repo-url: ~a" 
                build-url repo-url)
      (let ((build-info-dto (make-instance 'build-info
                                         :build-url build-url
                                         :repo-url repo-url)))
        (request api-context "/api/build-info"
                 :method :put
                 :content build-info-dto
                 :decode-response nil)))))

(defun upload-commit-graph/handler (cmd)
  (with-clingon-api-context (api-context cmd)
    (let ((run-context (make-instance 'commit-graph-run-context
                                      :repo-url (getopt cmd :repo-url)
                                      :main-branch (getopt cmd :main-branch))))
      (unless (run-context:repo-url run-context)
        (error "Could not determine repository URL, please pass the --repo-url argument"))
      (log:info "Updating commit graph for ~a" (run-context:repo-url run-context))
      (push-build-info-if-xcode-cloud api-context run-context)
      (update-commit-graph (make-instance 'commit-graph-updater :api-context api-context)
                           (run-context:git-repo run-context)
                           (run-context:main-branch run-context)))))

