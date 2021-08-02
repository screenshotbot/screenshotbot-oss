;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :quick-patch/impl
  (:use #:cl)
  (:import-from #:quick-patch/util
                #:trim
                #:catdir
                #:directory-exists-p)
  (:export
   #:*cache-dir*
   #:register-external
   #:prepare-externals))
(in-package :quick-patch/impl)

(defvar *externals* nil)

(defvar *cache-dir* nil)

(defun register-external (repo commit)
  (pushnew (cons repo commit)
           *externals*
           :test #'equal))

(defun run-program-with-errors (cmd)
  (multiple-value-bind (out err ret)
      (uiop:run-program cmd
                        :output 'string
                        :ignore-error-status t
                        :error-output 'string)
    (unless (eql 0 ret)
      (error "Bash command `~a` failed: ~%stdout: ~a~%~% stderr:~%~A~%"
             cmd
             out err))
    (trim out)))


(defun prepare-git-repo (repo commit cache-dir)
  (let ((git-dir (catdir cache-dir ".git/")))
   (cond
     ((directory-exists-p git-dir)
      (run-program-with-errors (list
                                "git" "--work-tree" (namestring cache-dir)
                                "--git-dir" (namestring git-dir)
                                "fetch"
                                repo)))
     (t
      (run-program-with-errors (list
                                "git" "clone"
                                repo
                                (namestring  cache-dir)))))

    ;; checkout the right commit
    (run-program-with-errors (list
                              "git" "--work-tree" (namestring cache-dir)
                              "--git-dir" (namestring git-dir)
                              "checkout"
                              commit))))

(defun name-from-repo-name (repo-name)
  (let ((pos (position #\/ repo-name :from-end t)))
    (subseq repo-name (+ 1 pos))))

(defun prepare-externals (cache-dir)
  (setf *cache-dir* cache-dir)
  (loop for (repo . commit) in *externals*
        do
           (let* ((name (name-from-repo-name repo))
                  (cache-dir (catdir *cache-dir* (format nil "~a/" name))))
             (pushnew cache-dir
                      asdf:*central-registry*
                      :test 'equal)
             (prepare-git-repo repo commit cache-dir))))
