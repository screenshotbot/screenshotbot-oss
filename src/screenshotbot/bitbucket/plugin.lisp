;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/pro/bitbucket/plugin
  (:nicknames :screenshotbot/bitbucket/plugin)
  (:use #:cl)
  (:import-from #:screenshotbot/plugin
                #:plugin-parse-repo
                #:plugin)
  (:import-from #:screenshotbot/installation
                #:find-plugin
                #:installation)
  (:import-from #:screenshotbot/user-api
                #:pull-request-url
                #:commit-link)
  (:import-from #:screenshotbot/github/access-checks
                #:get-repo-id)
  (:import-from #:screenshotbot/git-repo
                #:repo-link
                #:generic-git-repo)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:screenshotbot/dashboard/review-link
                #:describe-pull-request)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:bitbucket-plugin
   #:bitbucket-plugin-secret
   #:bitbucket-plugin-key
   #:bitbucket-repo
   #:bitbucket-repo-link))
(in-package :screenshotbot/bitbucket/plugin)

(with-class-validation
 (defclass bitbucket-plugin (plugin)
   ((key :initarg :key
         :reader bitbucket-plugin-key)
    (secret :initarg :secret
            :reader bitbucket-plugin-secret))))

(defun bitbucket-plugin (&key (installation (installation)))
  (find-plugin installation 'bitbucket-plugin))

(defclass bitbucket-repo (generic-git-repo)
  ())

(defmethod plugin-parse-repo ((plugin bitbucket-plugin)
                              company
                              repo-str)
  (when (str:containsp "bitbucket.org" repo-str)
    (make-instance 'bitbucket-repo :link repo-str
                                   :company company)))

(defun get-bitbucket-repo-id (repo)
  (cl-ppcre:regex-replace-all
   "^(git@bitbucket.org:|https://bitbucket.org/)([^.]*)([.]git)?$"
   repo
   "\\2"))

(defmethod commit-link ((repo bitbucket-repo) hash)
  (format nil "https://bitbucket.org/~a/commits/~a"
          (get-bitbucket-repo-id (repo-link repo))
          hash))

(defmethod describe-pull-request ((repo bitbucket-repo) run)
  (let ((url (pull-request-url run)))
    (multiple-value-bind (all parts)
        (cl-ppcre:scan-to-strings ".*/(pull-requests/(\\d)*)$" url)
      (cond
        (all
         (elt parts 0))
        (t
         (call-next-method))))))
