;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/gitlab/repo
    (:use #:cl
          #:alexandria
          #:screenshotbot/java
          #:screenshotbot/model/channel)
  (:import-from #:screenshotbot/model
                #:make-gitlab-repo)
  (:import-from #:screenshotbot/secret
                #:secret
                #:defsecret)
  (:import-from #:../git-repo
                #:generic-git-repo)
  (:export #:gitlab-repo
           #:project-id
           #:gitlab-api
           #:project-path
           #:make-gitlab-repo))


(named-readtables:in-readtable java-syntax)

(defclass gitlab-repo (generic-git-repo)
  ((link :initarg :link
         :accessor repo-link)
   (access-token :initarg :access-token
                 :accessor repo-access-token)))

(defun make-gitlab-repo (&href link)
  (make-instance 'gitlab-repo
                 :link link))

(defun pp (x)
  (log:info "Returning: ~S" x)
  x)

(defmethod project-path ((repo gitlab-repo))
  (str:substring 1 nil (elt (multiple-value-list (quri:parse-uri (repo-link repo))) 4)))

(defmethod project-project ((repo gitlab-repo))
  (destructuring-bind (namespace project)
      (str:split "/" (project-path repo))
    (#_getProject (#_getProjectApi (gitlab-api repo))
                  (pp namespace)
                  (pp project))))

(defmethod project-id ((repo gitlab-repo))
  (#_getId (project-project repo)))

(defsecret :gitlab-repo-access-token
  "Access token for GitLab API")

(defmethod gitlab-api ((repo gitlab-repo))
  (new-instance #,org.gitlab4j.api.GitLabApi
                (format nil
                        "https://~a"
                        (elt (multiple-value-list (quri:parse-uri (repo-link repo))) 2))
                (repo-access-token repo)))

(defmethod public-repo-p ((repo gitlab-repo))
  (restart-case
      (#_getPublic (project-project repo))
    (retry-public-repo-p ()
      (public-repo-p repo))))
