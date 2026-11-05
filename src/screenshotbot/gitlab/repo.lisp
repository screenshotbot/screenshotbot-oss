;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/gitlab/repo
  (:use #:cl
        #:alexandria
        #:screenshotbot/model/channel)
  (:import-from #:screenshotbot/model/channel
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
  (project-path (repo-link repo)))

(defmethod project-path ((repo-url string))
  (str:substring 1 nil (elt (multiple-value-list (quri:parse-uri repo-url)) 4)))

(defvar *public-repo-p-cache* (make-hash-table :test #'equal)
  "Map from project-path to (PUBLICP . CHECKED-AT).")

(defvar *public-repo-p-lock* (bt:make-lock "gitlab-public-repo-p"))

(defparameter *public-repo-p-ttl* 600
  "How long, in seconds, we trust a cached public-repo-p result. Every run
in a batch finalizes at about the same time, and without the cache each
of them makes its own unauthenticated GitLab request, which gets us
rate-limited.")

(defun %fetch-public-repo-p (repo)
  (restart-case
    (handler-case
        (let ((api (format nil "~a/projects/~a" *gitlab-url*
                           (urlencode:urlencode (project-path repo)))))
          (dex:get api)
          t)
      (dexador.error:http-request-not-found ()
        nil))
    (retry-public-repo-p ()
      (%fetch-public-repo-p repo))))

(defun cached-public-repo-p (repo &key (now (get-universal-time)))
  ;; Holding the lock across the request is intentional: concurrent
  ;; callers for the same repo wait for the first one's answer instead
  ;; of all hitting GitLab.
  (bt:with-lock-held (*public-repo-p-lock*)
    (let* ((key (project-path repo))
           (cached (gethash key *public-repo-p-cache*)))
      (cond
        ((and cached
              (< (- now (cdr cached)) *public-repo-p-ttl*))
         (car cached))
        (t
         (let ((publicp (%fetch-public-repo-p repo)))
           (setf (gethash key *public-repo-p-cache*)
                 (cons publicp now))
           publicp))))))

(defmethod public-repo-p ((repo gitlab-repo))
  (cached-public-repo-p repo))

(defmethod commit-link ((repo gitlab-repo)
                        hash)
  (format nil ""))
