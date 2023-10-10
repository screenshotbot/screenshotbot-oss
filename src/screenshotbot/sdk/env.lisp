;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/env
  (:use #:cl)
  (:import-from #:alexandria
                #:assoc-value
                #:when-let
                #:if-let)
  (:import-from #:screenshotbot/sdk/git
                #:git-message
                #:git-repo)
  (:shadow #:getenv)
  (:export
   #:make-env-reader
   #:api-key
   #:api-secret
   #:api-hostname
   #:pull-request-url
   #:sha1
   #:build-url
   #:repo-url
   #:guess-channel-name
   #:work-branch))
(in-package :screenshotbot/sdk/env)

(defclass base-env-reader ()
  ((overrides :initarg :overrides
              :reader overrides))
  (:documentation "Reads the environment to get some information about the current build"))

(defgeneric work-branch (env)
  (:documentation "This isn't being used, and can be removed if required. Currently we
get this from the Git repository directly."))

(defclass env-reader (base-env-reader)
  ())

(defmethod getenv ((self base-env-reader) name)
  (if (slot-boundp self 'overrides)
   (assoc-value (overrides self)
                name :test #'string=)
   (uiop:getenv name)))

(defmethod api-key ((self base-env-reader))
  (getenv self "SCREENSHOTBOT_API_KEY"))

(defmethod api-secret ((self base-env-reader))
  (getenv self "SCREENSHOTBOT_API_SECRET"))

(defmethod api-hostname ((self base-env-reader))
  (getenv self "SCREENSHOTBOT_API_HOSTNAME"))

(defmethod pull-request-url ((self env-reader))
  nil)

(defmethod guess-channel-name ((self base-env-reader))
  nil)

(defmethod work-branch ((self env-reader))
  nil)

(defmethod build-url ((self env-reader))
  ;; For jenkins
  (getenv self "BUILD_URL"))

(defmethod repo-url ((self env-reader))
  nil)

(defmethod sha1 ((self env-reader))
  nil)

(defgeneric validp (env-reader)
  (:documentation "Is the current environment reader valid for this situation"))

(defclass circleci-env-reader (base-env-reader)
  ())

(defmethod build-url ((self circleci-env-reader))
  (getenv self "CIRCLE_BUILD_URL"))

(defmethod repo-url ((self circleci-env-reader))
  (getenv self "CIRCLE_REPOSITORY_URL"))

(defmethod validp ((self circleci-env-reader))
  (or
   (sha1 self)
   (pull-request-url self)))

(defmethod pull-request-url ((self circleci-env-reader))
  (getenv self "CIRCLE_PULL_REQUEST"))

(defmethod sha1 ((self circleci-env-reader))
  (getenv self "CIRCLE_SHA1"))

(defmethod work-branch ((self circleci-env-reader))
  (getenv self "CIRCLE_BRANCH"))

(defclass bitrise-env-reader (base-env-reader)
  ())

(defmethod validp ((self bitrise-env-reader))
  (or
   (pull-request-url self)
   (sha1 self)))

(defmethod build-url ((self bitrise-env-reader))
  (getenv self "BITRISE_BUILD_URL"))

(defmethod pull-request-url ((self bitrise-env-reader))
  (if-let ((repo-url (repo-url self))
           (pull-id (getenv self "BITRISE_PULL_REQUEST")))
    (link-to-github-pull-request repo-url pull-id)))

(defmethod repo-url ((Self bitrise-env-reader))
  (or
   ;; TODO: this one is probably incorrect.
   (getenv self "BITRISEIO_PULL_REQUEST_REPOSITORY_URL")
   (getenv self "GIT_REPOSITORY_URL")))

(defmethod work-branch ((self bitrise-env-reader))
  (getenv self "BITRISE_GIT_BRANCH"))

(defun link-to-github-pull-request (repo-url pull-id)
  (let ((key (cond
               ((str:containsp "bitbucket" repo-url)
                "pull-requests")
               (t
                "pulls"))))
   (format nil "~a/~a/~a"
           repo-url
           key
           pull-id)))

(defclass netlify-env-reader (base-env-reader)
  ())

(defmethod pull-request-url ((self netlify-env-reader))
  (let ((pull-request-p (equal "true" (getenv self "PULL_REQUEST"))))
    (when pull-request-p
      (when-let ((review-id (getenv self "REVIEW_ID")))
        (link-to-github-pull-request
         (repo-url self)
         review-id)))))

(defmethod build-url ((self netlify-env-reader))
  (let ((build-id (getenv self "BUILD_ID"))
        (site-name (getenv self "SITE_NAME")))
    (format nil "https://app.netlify.com/sites/~a/deploys/~a"
            site-name
            build-id)))

(defmethod repo-url ((self netlify-env-reader))
  (getenv self "REPOSITORY_URL"))

(defmethod guess-channel-name ((self netlify-env-reader))
  (getenv self "SITE_NAME"))

(defmethod sha1 ((self netlify-env-reader))
  (getenv self "COMMIT_REF"))

(defmethod validp ((self netlify-env-reader))
  (equal "true" (getenv self "NETLIFY")))

(defmethod validp ((self env-reader))
  t)

(defmethod work-branch ((self netlify-env-reader))
  (getenv self "BRANCH"))

(defmethod sha1 ((self bitrise-env-reader))
  (getenv self "BITRISE_GIT_COMMIT"))

(defclass azure-env-reader (base-env-reader)
  ())

(defmethod getenv ((self azure-env-reader) name)
  "Make it convenient to copy paste environment variables from Azure docs"
  (call-next-method
   self
   (str:upcase (str:replace-all "." "_" name))))

(defmethod validp ((self azure-env-reader))
  (getenv self "Build.BuildId"))

(defmethod pull-request-url ((self azure-env-reader))
  (when-let ((repo-url (getenv self "System.PullRequest.SourceRepositoryURI"))
             (pull-id (getenv self "System.PullRequest.PullRequestId")))
   (format nil "~a/pullrequest/~a"
           repo-url
           pull-id)))

(defmethod sha1 ((self azure-env-reader))
  (getenv self "Build.SourceVersion"))

(defmethod build-url ((self azure-env-reader))
  ;; This does not return an HTTPS url sadly...
  (getenv self "Build.BuildUri"))

(defmethod repo-url ((self azure-env-reader))
  (getenv self "Build.Repository.Uri"))

(defmethod work-branch ((self azure-env-reader))
  (getenv self "Build.SourceBranchName"))

;; https://buildkite.com/docs/pipelines/environment-variables
(defclass buildkite-env-reader (base-env-reader)
  ())

(defmethod validp ((self buildkite-env-reader))
  (getenv self "BUILDKITE_BUILD_ID"))

(defmethod make-pr-url (self repo pr-id)
  (when-let ((repo-url (getenv self repo))
             (pull-id (getenv self pr-id)))
    (format nil "~a/pull/~a"
            repo-url
            pull-id)))

(defmethod pull-request-url ((self  buildkite-env-reader))
  (let ((pr "BUILDKITE_PULL_REQUEST"))
   (unless (string-equal "false" (getenv self pr))
     (make-pr-url self
                  "BUILDKITE_REPO"
                  pr))))

(defmethod sha1 ((self buildkite-env-reader))
  (getenv self "BUILDKITE_COMMIT"))

(defmethod build-url ((self buildkite-env-reader))
  (getenv self "BUILDKITE_BUILD_URL"))

(defmethod repo-url ((Self buildkite-env-reader))
  (getenv self "BUILDKITE_REPO"))

(defmethod work-branch ((self buildkite-env-reader))
  (getenv self "BUILDKITE_BRANCH"))

(defclass bitbucket-pipeline-env-reader (base-env-reader)
  ())

(defmethod validp ((self bitbucket-pipeline-env-reader))
  (getenv self "BITBUCKET_BUILD_NUMBER"))

(defmethod pull-request-url ((self bitbucket-pipeline-env-reader))
  (make-pr-url
   self
   "BITBUCKET_GIT_HTTP_ORIGIN"
   "BITBUCKET_PR_ID"))

(defmethod sha1 ((self bitbucket-pipeline-env-reader))
  (getenv self "BITBUCKET_COMMIT"))

(defmethod build-url ((self bitbucket-pipeline-env-reader))
  (format nil "~a/addon/pipelines/home#!/results/~a"
          (getenv self "BITBUCKET_GIT_HTTP_ORIGIN")
          (getenv self "BITBUCKET_BUILD_NUMBER")))

(defmethod repo-url ((self bitbucket-pipeline-env-reader))
  (getenv self "BITBUCKET_GIT_HTTP_ORIGIN"))

(defmethod work-branch ((self bitbucket-pipeline-env-reader))
  (getenv self "BITBUCKET_BRANCH"))

(defclass gitlab-ci-env-reader (base-env-reader)
  ())

(defmethod validp ((self gitlab-ci-env-reader))
  (getenv self "GITLAB_CI"))

(defmethod pull-request-url ((self gitlab-ci-env-reader))
  (format nil "~a/-/merge_requests/~a"
          (getenv self "CI_MERGE_REQUEST_PROJECT_URL")
          (getenv self "CI_MERGE_REQUEST_IID")))

(defmethod sha1 ((self gitlab-ci-env-reader))
  (getenv self "CI_COMMIT_SHA"))

(defmethod build-url ((self gitlab-ci-env-reader))
  (getenv self "CI_JOB_URL"))

(defmethod repo-url ((self gitlab-ci-env-reader))
  (getenv self "CI_PROJECT_URL"))

(defmethod work-branch ((self gitlab-ci-env-reader))
  (getenv self "CI_COMMIT_REF_NAME"))

(defclass github-actions-env-reader (base-env-reader)
  ())

(defmethod validp ((self github-actions-env-reader))
  (getenv self "GITHUB_ACTION"))

(defmethod pull-request-url ((self github-actions-env-reader))
  nil)

(defmethod sha1 ((self github-actions-env-reader))
  (let ((sha (getenv self "GITHUB_SHA")))
    (let* ((repo (make-instance 'git-repo))
           (message (git-message repo)))
      (multiple-value-bind (res parts)
          (cl-ppcre:scan-to-strings
           ".*Merge (.*) into .*"
           message)
        (cond
          (res
           (elt parts 0))
          (t
           (warn "Could not parse ~a" message)
           sha))))))

(defmethod github-server-url ((self github-actions-env-reader))
  (getenv self "GITHUB_SERVER_URL"))

(defmethod github-repo ((self github-actions-env-reader))
  (getenv self "GITHUB_REPOSITORY"))

(defmethod build-url ((self github-actions-env-reader))
  (format nil "~a/~a/actions/runs/~a"
          (github-server-url self)
          (github-repo self)
          (getenv self "GITHUB_RUN_ID")))

(defmethod repo-url ((self github-actions-env-reader))
  (format nil
          "~a/~a"
          (github-server-url self)
          (github-repo self)))

(defmethod work-branch ((self github-actions-env-reader))
  (getenv self "GITHUB_BASE_REF"))

(defparameter *all-readers*
  '(circleci-env-reader
    bitrise-env-reader
    netlify-env-reader
    azure-env-reader
    bitbucket-pipeline-env-reader
    buildkite-env-reader
    gitlab-ci-env-reader
    github-actions-env-reader))


(defun make-env-reader (&key overrides)
  (loop for option in *all-readers*
        for env = (make-instance option)
        if (validp env)
          return env
        finally
           (return (make-instance 'env-reader))))
