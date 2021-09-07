;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/gitlab/repo
  (:use #:cl
        #:alexandria
        #:screenshotbot/model/channel)
  (:import-from #:screenshotbot/model
                #:make-gitlab-repo)
  (:import-from #:screenshotbot/secret
                #:secret
                #:defsecret)
  (:import-from #:screenshotbot/git-repo
                #:public-repo-p
                #:generic-git-repo)
  (:export
   #:gitlab-repo
   #:project-path
   #:make-gitlab-repo
   #:*gitlab-url*))
(in-package :screenshotbot/gitlab/repo)

;; TODO: move this into a config
(defvar *gitlab-url* "https://gitlab.com/api/v4")

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


(defsecret :gitlab-repo-access-token
  "Access token for GitLab API")

(defmethod public-repo-p ((repo gitlab-repo))
  (restart-case
    (handler-case
        (let ((api (format nil "~a/projects/~a" *gitlab-url*
                           (urlencode:urlencode (project-path repo)))))
          (dex:get api))
      (dexador.error:http-request-not-found ()
        nil))
    (retry-public-repo-p ()
      (public-repo-p repo))))
