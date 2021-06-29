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
  (:export #:gitlab-repo
           #:project-id
           #:gitlab-api
           #:project-path
           #:make-gitlab-repo))


(named-readtables:in-readtable java-syntax)

(defclass gitlab-repo ()
  ((link :initarg :link
         :accessor repo-link)
   (access-token :initarg :access-token
                 :accessor gitlab-access-token)))

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

(defmethod repo-access-token ((repo gitlab-repo))
  (log:warn "unimplemented: repo-access-token")
  (secret :gitlab-repo-access-token))

(defmethod gitlab-api ((repo gitlab-repo))
  (new-instance #,org.gitlab4j.api.GitLabApi
                (format nil
                        "https://~a"
                        (elt (multiple-value-list (quri:parse-uri (repo-link repo))) 2))
                (repo-access-token repo)))
(defun merge-base (repo test head)
  (check-type test string)
  (check-type head string)
  (let ((refs (new-instance #,java.util.ArrayList)))
    (#_add refs test)
    (#_add refs head)
    (#_getMergeBase (#_getRepositoryApi (gitlab-api repo))
                    (project-id repo)
                    refs)))

(defmethod repo-ancestor-p ((repo gitlab-repo) test head)
  (check-type test string)
  (check-type head string)
  (restart-case
      (let ((merge-base (merge-base repo test head)))
        (equal (#_getId merge-base)
               (#_getId (get-commit repo test))))
    (restart-repo-ancestor-p ()0
      (repo-ancestor-p repo test head))))

(defmethod repo-left-ancestor-p ((repo gitlab-repo) test head)
  (restart-case
      (labels ((left-anc-r (test head)
                 #+lispworks
                 (check-type test lw-ji:jobject)
                 #+lispworks
                 (check-type head lw-ji:jobject)
                 (cond
                   ((not (repo-ancestor-p repo (#_getId test) (#_getId head)))
                    nil)
                   ((equal (#_getId test) (#_getId head))
                    t)
                   (t
                    ;; we know test is a strict ancestor of head.
                    (left-anc-r test
                                (get-commit repo (get-parent-commit repo (#_getId head))))))))
        (left-anc-r (get-commit repo test)
                    (get-commit repo head)))
    (restart-repo-left-ancestor-p ()
      (repo-left-ancestor-p repo test head))))

(defun get-commit (repo commit)
  (etypecase commit
    (string
     (let ((api (#_getCommitsApi (gitlab-api repo))))
       (#_getCommit
        api
        (project-id repo)
        commit)))
    (t
     commit)))

(defmethod get-parent-commit ((repo gitlab-repo) commit)
  (restart-case
      (progn
        (let ((commit (etypecase commit
                        (string (get-commit repo commit))
                        (t commit))))
          (car
           (java-list->list
            (#_getParentIds commit)))))
    (get-parent-commit-again ()
      (get-parent-commit repo commit))))

(defmethod public-repo-p ((repo gitlab-repo))
  (restart-case
      (#_getPublic (project-project repo))
    (retry-public-repo-p ()
      (public-repo-p repo))))
