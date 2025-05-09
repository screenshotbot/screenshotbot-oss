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
                #:repo-link
                #:get-remote-url
                #:author
                #:fetch-remote-branch
                #:git-root
                #:git-message
                #:read-graph
                #:rev-parse
                #:current-commit
                #:current-branch
                #:git-command
                #:repo-dir
                #:git-repo
                #:$)
  (:import-from #:fiveam-matchers/errors
                #:signals-error-matching
                #:error-with-string-matching)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:export
   #:with-git-repo
   #:make-commit
   #:enable-server-features))
(in-package :screenshotbot/sdk/test-git)

(util/fiveam:def-suite)

(defun run-in-dir (repo cmd &rest args)
  (let ((cmd (cl-ppcre:regex-replace
              "^git " cmd
              "git -c user.name=FooBar -c user.email=foo@example.com ")))
    (apply #'uiop:run-program (format nil "cd ~a && ~a" (namestring (repo-dir repo)) cmd)
           args)))

(defun enable-server-features (repo)
  (loop for feature in (list "filter"
                             "allowTipSHA1InWant"
                             "allowReachableSHA1InWant")
        do
        (run-in-dir repo (format nil "git config uploadpack.~a true" feature))))

(defun make-commit (repo content)
  (let ((file (path:catfile (repo-dir repo) "file.txt")))
   (with-open-file (stream file
                           :direction :output
                           :if-exists :supersede)
     (write-string content stream))
    (run-in-dir repo "git add file.txt" )
    (run-in-dir repo "git commit -a -m ...")))

(defun make-directory-deletable (dir)
  "On windows, git might have read-only files"
  (declare (ignorable dir))
  #+(or windows mswindows)
  (uiop:run-program (list "attrib" "-r" (namestring (path:catfile dir "*.*")) "/s")))

(def-easy-macro with-tmpdir (&binding dir &fn fn)
  (tmpdir:with-tmpdir (dir)
    (unwind-protect
         (fn dir)
      (make-directory-deletable dir))))

(def-easy-macro with-git-repo (&binding repo &key link &binding dir &fn fn)
  (with-tmpdir (dir)
    (progn
      (uiop:run-program (list "git" "init" (namestring dir)))
      (let ((repo (make-instance 'git-repo :dir dir :link link)))
        (fn repo dir)))))

(def-fixture git-repo ()
  #-screenshotbot-oss
  (with-git-repo (repo :dir dir)
    (&body)))

(test get-current-commit
  (with-fixture git-repo ()
    (make-commit repo "foobar")
    (assert-that (current-commit repo)
                 (has-length 40))))

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

(test rev-parse-happy-path
  (with-fixture git-repo ()
    (make-commit repo "foobar")
    (run-in-dir repo "git checkout -b origin/car")
    (is-false (rev-parse repo "bleh"))
    (is-true
     (rev-parse repo "car"))))

(def-easy-macro with-clone (&binding clone repo &fn fn)
  (with-tmpdir (clone-root)
    (run-in-dir repo "git checkout -b master")
    (make-commit repo "foobar")
    (uiop:run-program (format nil "cd ~a && git clone ~a cloned-repo"
                              clone-root
                              (namestring (repo-dir repo)))
                      :error-output *standard-output*)
    (let ((clone (make-instance 'git-repo :dir (path:catdir clone-root "cloned-repo/"))))
      (fn clone))))

(test fetch-remote-branch
  (with-fixture git-repo ()
    (with-clone (clone repo)
      (make-commit repo "new-commit")
      (let ((rev (str:trim (run-in-dir repo "git rev-parse HEAD"
                                       :output 'string))))
        (is (not (str:emptyp rev)))
        (fetch-remote-branch clone "master")
        (is (equal rev
                   (rev-parse clone "master")))))))

(test read-dag-from-repo
  (with-fixture git-repo ()
    (make-commit repo "foobar")
    (let ((dag
            (read-graph repo)))
      (assert-that (dag::all-commits dag)
                   (has-length 1)))))

(test git-message
  (with-fixture git-repo ()
    (make-commit repo "foobar")
    (is (equal "..." (git-message repo)))))

(test get-git-root
  (with-tmpdir (dir)
    (ensure-directories-exist (path:catdir dir "foo/bar/car/"))
    (ensure-directories-exist (path:catdir dir "foo/.git/"))
    (ensure-directories-exist (path:catdir dir "car/dar/"))
    (is (equalp
         (path:catdir dir "foo/")
         (git-root :directory (path:catdir dir "foo/bar/car/") :errorp t)))
    (is (equalp
         (path:catdir dir "foo/")
         (git-root :directory (path:catdir dir "foo/") :errorp t)))
    (is (equalp
         nil
         (git-root :directory (path:catdir dir "car/dar/") :errorp nil)))
    (signals-error-matching
        ()
        (git-root :directory (path:catdir dir "car/dar/") :errorp t)
        (error-with-string-matching "Could not find git root"))))

(test get-git-author
  (with-fixture git-repo ()
    (make-commit repo "foobar")
    (make-commit repo "bleh")
    (is (equal "foo@example.com" (author repo)))))

(test get-remote-url
  (with-fixture git-repo ()
    (is (equal nil (get-remote-url repo)))
    (with-clone (clone repo)
      (is (equal (namestring dir)
                 (namestring (get-remote-url clone)))))))
