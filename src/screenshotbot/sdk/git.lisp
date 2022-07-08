;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/sdk/git
  (:use #:cl
        #:alexandria)
  (:export
   #:null-repo
   #:git-repo
   #:rev-parse
   #:merge-base
   #:current-commit))
(in-package :screenshotbot/sdk/git)

(defun git-root ()
  (pathname (format nil "~a/"
                    ($ "git" "rev-parse" "--show-toplevel"))))

(defclass git-repo ()
  ((link :initarg :link
         :accessor repo-link)
   (dir :initarg :dir
        :accessor repo-dir
        :initform (git-root))))

(defclass null-repo ()
  ())

(defmethod repo-link ((repo null-repo))
  nil)

(defmethod cleanp ((repo null-repo))
  t)

(defmethod git-command ((repo git-repo))
  (list
   "git"
   "--git-dir"
   (namestring (path:catdir (repo-dir repo) ".git/"))))

(defmethod cleanp ((repo git-repo))
  (str:emptyp ($ (git-command repo) "status" "--porcelain")))


(defmethod current-commit ((repo git-repo))
  ($ (git-command repo) "rev-parse" "HEAD"))

(defmethod current-commit ((repo null-repo))
  nil)

(defun $ (&rest args)
  (let* ((args (flatten args))
         (args (loop for arg in args
                    if (pathnamep arg)
                      collect (namestring arg)
                    else
                      collect arg))
         (out
           (uiop:run-program args
                             :output 'string
                             :error-output *error-output*)))
    (str:trim out)))

(defmethod read-graph ((repo git-repo))
  (let ((lines (nreverse (str:lines
                          ($ (git-command repo)
                            "log" "--all" "--pretty=%H %P")))))
    (let ((dag (make-instance 'dag:dag)))
      (dolist (line lines)
        (destructuring-bind (sha &rest parents) (str:split " " line)
          (dag:add-commit dag (make-instance 'dag:commit
                                             :sha sha
                                             :parents parents))))
      dag)))

(defmethod merge-base ((repo git-repo) master-sha commit-sha)
  ($ (git-command repo) "merge-base" master-sha commit-sha))

(defmethod merge-base ((repo null-repo) master-sha commit-sha)
  nil)

(defmethod rev-parse ((repo git-repo) branch)
  (handler-case
      ($ (git-command repo) "rev-parse" (format nil "origin/~a" branch))
    (error
      nil)))

(defmethod rev-parse ((repo null-repo) branch)
  nil)
