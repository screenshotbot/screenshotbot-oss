;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/gitlab/repo
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
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/user-api
                #:commit-link)
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
   (company :initarg :company
            :accessor company)))

(defun make-gitlab-repo (&href link)
  (make-instance 'gitlab-repo
                 :link link))

(defmethod project-path ((repo gitlab-repo))
  (str:substring 1 nil (elt (multiple-value-list (quri:parse-uri (repo-link repo))) 4)))

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

(defmethod commit-link ((repo gitlab-repo)
                        hash)
  (format nil ""))
