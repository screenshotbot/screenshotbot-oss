;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/sdk/git
  (:use #:cl
        #:alexandria)
  (:import-from #:util/health-check
                #:def-health-check)
  (:import-from #:dag
                #:ordered-commits)
  (:export
   #:null-repo
   #:git-repo
   #:rev-parse
   #:merge-base
   #:current-commit
   #:current-branch
   #:cleanp)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags)))
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
  (log:debug "Running command with args: ~S" args)
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

(defmethod current-branch ((repo git-repo))
  ($ (git-command repo) "rev-parse" "--abbrev-ref" "HEAD"))

(defmethod current-branch ((repo null-repo))
  nil)

(defmethod read-graph ((repo git-repo))
  (dag:read-from-stream
   (make-string-input-stream
    ($ (git-command repo)
      "log" "--all"
      (when flags:*commit-limit*
        (format nil "--max-count=~a" flags:*commit-limit*))
      "--pretty=%H %P"))
   :format :text))

(defmethod merge-base ((repo git-repo) master-sha commit-sha)
  ($ (git-command repo) "merge-base" master-sha commit-sha))

(defmethod merge-base ((repo null-repo) master-sha commit-sha)
  nil)

(defmethod rev-parse ((repo git-repo) branch)
  (handler-case
      ($ (git-command repo) "rev-parse" "-q" "--verify" (format nil "origin/~a" branch))
    (error
      nil)))

(defmethod rev-parse ((repo null-repo) branch)
  nil)


(def-health-check verify-git-is-present ()
  (uiop:run-program (list "git" "--help")))

(def-health-check verify-can-read-a-git-commit-graph ()
  (tmpdir:with-tmpdir (dir)
    (let ((*error-output* (make-string-output-stream)))
      ($ "git" "clone" "https://github.com/tdrhq/tiny-test-repo" dir))
    (let ((repo (make-instance 'git-repo :dir dir)))
      (let ((dag (read-graph repo)))
        (assert (> (length (dag:ordered-commits dag)) 0))))

    ;; On Windows, some Git files are read-only, which will make this test
    ;; fail
    #+windows
    (uiop:run-program (list "attrib" "-r" (format nil "~a\\*.*" (namestring dir)) "/s"))))
