;; -*- encoding: utf-8 -*-
;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-git
            (:use #:cl
                  #:fiveam)
            (:import-from #:screenshotbot/sdk/git
                          #:current-commit
                          #:current-branch
                          #:git-command
                          #:repo-dir
                          #:git-repo
                          #:$
                          #:assert-commit)
            (:import-from #:fiveam-matchers/core
                          #:assert-that)
            (:import-from #:fiveam-matchers/strings
                          #:contains-string)
            (:import-from #:fiveam-matchers/has-length
                          #:has-length))
(in-package :screenshotbot/sdk/test-git)

(util/fiveam:def-suite)

(defun run-in-dir (repo cmd)
  (uiop:run-program (format nil "cd ~a && ~a" (namestring (repo-dir repo)) cmd)))

(defun make-commit (repo content)
  (let ((file (path:catfile (repo-dir repo) "file.txt")))
   (with-open-file (stream file
                           :direction :output)
     (write-string content stream))
    (run-in-dir repo "git add file.txt" )
    (run-in-dir repo "git commit -a -m ...")))

(def-fixture git-repo ()
  (tmpdir:with-tmpdir (dir)
    (uiop:run-program (list "git" "init" (namestring dir)))
    (let ((repo (make-instance 'git-repo :dir dir)))
      (&body))))

(test assert-commit
  (handler-case
      (progn
        (assert-commit "foo" "foo bar")
        (fail "expected error"))
    (error (e)
      (assert-that (format nil "~a" e)
                   (contains-string "`foo` does not")))))

#-screenshotbot-oss
(test get-current-commit
  (with-fixture git-repo ()
    (make-commit repo "foobar")
    (assert-that (current-commit repo)
                 (has-length 40))))

#-screenshotbot-oss
(test get-current-branch
  (with-fixture git-repo ()
    (make-commit repo "foobar")
    (run-in-dir repo "git checkout -b carbar")
    (is (equal "carbar"
               (current-branch repo)))))

(test utf-8-characters-in-git-branch
  (with-fixture git-repo ()
    (make-commit repo "foobar")
    (run-in-dir repo "git checkout -b léquipe")
    (let ((branch (current-branch repo)))
      (is (equal "léquipe" (current-branch repo))))))
