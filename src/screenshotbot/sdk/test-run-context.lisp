;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-run-context
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/run-context
                #:fix-commit-hash
                #:run-context-dto
                #:run-context-to-dto
                #:work-branch-is-release-branch-p
                #:run-context-metadata
                #:parse-shard-spec
                #:with-flags-from-run-context
                #:flags-run-context
                #:run-context
                #:env-reader-run-context
                #:invalid-pull-request
                #:validate-pull-request)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:screenshotbot/sdk/env
                #:env-reader
                #:make-env-reader)
  (:import-from #:cl-mock
                #:answer
                #:if-called)
  (:import-from #:fiveam-matchers/errors
                #:signals-error-matching
                #:error-with-string-matching)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex
                #:contains-string)
  (:local-nicknames (#:run-context #:screenshotbot/sdk/run-context)
                    (#:git #:screenshotbot/sdk/git)
                    (#:dto #:screenshotbot/api/model)
                    (#:flags #:screenshotbot/sdk/flags)
                    (#:test-git #:screenshotbot/sdk/test-git)))
(in-package :screenshotbot/sdk/test-run-context)


(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
   (&body)))

(defvar *env-overrides* nil)

(defun fake-getenv (name)
  (assoc-value *env-overrides* name :test #'equal))

(defmacro with-env (bindings &body body)
  `(util:copying (*env-overrides*)
     ,@ (loop for (name var) in bindings
              collect
              `(push (cons (str:replace-all "-" "_" (string ,name)) ,var)
                     *env-overrides*))
     (cl-mock:if-called 'uiop:getenv #'fake-getenv)
     ,@body))

(test validate-pull-request
  (with-fixture state ()
    (flet ((%run (url)
             (validate-pull-request url)))
      (handler-case
          (%run nil)
        (invalid-pull-request ()
          (fail "Saw warning for nil")))
      (signals invalid-pull-request
        (%run "git@github.com:trhq/fast-example.git"))
      (is (equal nil (%run "git@github.com:tdrhq/fast-example.git")))
      (is (equal "https://github.com/tdrhq/fast-example/pulls/1" (%run "https://github.com/tdrhq/fast-example/pulls/1"))))))

(defclass test-run-context (env-reader-run-context
                            run-context)
  ()
  (:default-initargs :env (make-env-reader)))

(test parse-override-commit-hash-in-run-context
  (with-fixture state ()
    (with-env ((:circle-pull-request "http://foo"))
      (is (equal "http://foo"
                 (run-context:pull-request-url
                  (make-instance 'test-run-context
                                 :env (make-env-reader)))))))
  (with-fixture state ()
    (with-env ((:circle-sha1 "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd")
               (:circle-pull-request "http://foo"))
      (is (equal "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
                 (run-context:override-commit-hash
                  (make-instance 'test-run-context
                                 :env (make-env-reader)))))))
  (with-fixture state ()
    (with-env ((:circle-sha1 "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd")
               (:circle-pull-request ""))
      (is (equal "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
                 (run-context:override-commit-hash
                  (make-instance 'test-run-context
                                 :env (make-env-reader))))))))


(test parse-override-commit-hash-with-bitrise
  (with-fixture state ()
    (with-env ((:bitrise-git-commit "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd")
               (:bitriseio-pull-request-repository-url "https://bitbucket.org/tdrhq/fast-example"))
      (is (equal nil (run-context:pull-request-url
                      (make-instance 'test-run-context))))
      (is (equal "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd" (run-context:override-commit-hash
                         (make-instance 'test-run-context))))))

  (with-fixture state ()
    (with-env ((:bitrise-git-commit "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd")
               (:bitrise-pull-request "1")
               (:bitriseio-pull-request-repository-url "https://bitbucket.org/tdrhq/fast-example"))

      (is (equal "https://bitbucket.org/tdrhq/fast-example/pull-requests/1"
                 (run-context:pull-request-url
                  (make-instance 'test-run-context))))

      (is (equal "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
                 (run-context:override-commit-hash
                  (make-instance 'test-run-context)))))))

(defclass my-run-context (run-context
                          env-reader-run-context)
  ())

(test env-reader-happy-paths
  (with-fixture state ()
    (if-called 'git:merge-base
               (lambda (repo main commit)
                 "foo"))
    (let ((self (make-instance 'my-run-context
                               :env (make-instance 'env-reader))))
      (finishes (run-context:build-url self))
      (finishes (run-context:repo-url self))
      (finishes (run-context:work-branch self))
      (finishes (run-context:pull-request-url self))
      (finishes (run-context:channel self))
      (finishes (run-context:main-branch self))
      (finishes (run-context:override-commit-hash self))
      (finishes (run-context:commit-hash self))
      (finishes (run-context:merge-base self))
      (finishes (run-context:repo-clean-p self))
      (finishes (run-context:main-branch-hash self)))))

(test main-branch-hash-for-flags
  (with-fixture state ()
    (let ((flags:*main-branch-commit-hash* "foo"))
      (is (equal "foo"
                 (run-context:main-branch-hash (make-instance 'flags-run-context)))))))

(test with-flags
  (with-fixture state ()
    (let ((rc (make-instance 'run-context
                             :channel "foobar")))
      (with-flags-from-run-context (rc)
        ;; just test one thing for now
        (is (equal "foobar" flags:*channel*))))))

(test copies-work-branch
  (with-fixture state ()
    (let ((flags:*work-branch* "foo"))
      (is (equal "foo"
                 (run-context:work-branch (make-instance 'flags-run-context)))))))

(test parse-shard-spec
  (with-fixture state ()
    (let ((spec (parse-shard-spec "foo:1:3")))
      (is (equal "foo" (dto:shard-spec-key spec)))
      (is (equal 1 (dto:shard-spec-number spec)))
      (is (equal 3 (dto:shard-spec-count spec))))))

(test metadata-happy-path
  (with-fixture state ()
    (let* ((run-context (make-instance 'test-run-context))
           (metadata (run-context-metadata run-context)))
      #-windows
      (assert-that
       metadata
       (has-length 2))
      #+linux
      (assert-that
       (dto:metadata-value (second metadata))
       (contains-string "Linux"))
      #+darwin
      (assert-that
       (dto:metadata-value (second metadata))
       (contains-string "Darwin")))))

(test uname-should-not-crash
  (with-fixture state ()
    (let ((run-context (make-instance 'test-run-context)))
      (if-called 'uiop:run-program
                 (lambda (&rest args)
                   (error "uname crashed!")))
      (assert-that
       (run-context-metadata run-context)
       (has-length 2)))))

(test release-branch-regex-should-fix-main-branch--old-test!
  ;; There was a time before T1667, when we were converting the
  ;; main-branch to release-branch on the CLI side, it should be done
  ;; on the server side now.
  (with-fixture state ()
    (let ((run-context (make-instance 'test-run-context
                                      :main-branch "foo")))
      (is (equal "foo" (run-context:main-branch run-context))))
    (let ((run-context (make-instance 'test-run-context
                                      :main-branch "foo"
                                      :work-branch "bar")))
      (is (equal "foo" (run-context:main-branch run-context))))
    (let ((run-context (make-instance 'test-run-context
                                      :main-branch "foo"
                                      :work-branch "bar"
                                      :release-branch-regex "b.*")))
      (is-true (run-context:work-branch-is-release-branch-p run-context))
      (is (equal "foo" (run-context:main-branch run-context))))

    (let ((run-context (make-instance 'test-run-context
                                      :main-branch "foo"
                                      :work-branch "bar"
                                      :release-branch-regex "c.*")))
      (is-false (run-context:work-branch-is-release-branch-p run-context))      
      (is (equal "foo" (run-context:main-branch run-context))))))

(test work-branch-is-release-branch-p
  (let ((run-context (make-instance 'test-run-context
                                    :work-branch "foo"
                                    :release-branch-regex "b.*")))
    (is-false (work-branch-is-release-branch-p run-context)))
  (let ((run-context (make-instance 'test-run-context
                                    :work-branch "foo"
                                    :release-branch-regex "f.*")))
    (is-true (work-branch-is-release-branch-p run-context)))
  (let ((run-context (make-instance 'test-run-context
                                    :work-branch "foo"
                                    :release-branch-regex "")))
    (is-false (work-branch-is-release-branch-p run-context))))

(test work-branch-is-release-branch-p--matches-full-branch
  (let ((run-context (make-instance 'test-run-context
                                    :work-branch "foo"
                                    :release-branch-regex "o")))
    (is-false (work-branch-is-release-branch-p run-context))))

(test invalid-regex
  (let ((run-context (make-instance 'test-run-context
                                    :work-branch "foo"
                                    :release-branch-regex "foo(")))
    (signals-error-matching ()
       (work-branch-is-release-branch-p run-context)
       (error-with-string-matching (matches-regex "Could not parse regex")))))

(test regex-with-parenthesis
  "Keep this test in sync with the documentation for *release-branch-regex*, this is just
making sure that the doc is giving a good example."
  (let ((regex "(release|long-feature)/.*"))
    (let ((run-context (make-instance 'test-run-context
                                      :work-branch "release/2025-02"
                                      :release-branch-regex regex)))
      (is-true (work-branch-is-release-branch-p run-context)))
    (let ((run-context (make-instance 'test-run-context
                                      :work-branch "long-feature/big-thing"
                                      :release-branch-regex regex)))
      (is-true (work-branch-is-release-branch-p run-context)))
    (let ((run-context (make-instance 'test-run-context
                                      :work-branch "main"
                                      :release-branch-regex regex)))
      (is-false (work-branch-is-release-branch-p run-context)))))


(test simple-dto-conversion
  (let ((dto
          (run-context-to-dto
           (make-instance 'test-run-context
                          :work-branch "foo"
                          :env (make-env-reader)))))
    (is (typep dto 'run-context-dto))
    (is (equal "foo" (run-context:work-branch dto)))))

(test merge-base-handles-subprocess-error
  (with-fixture state ()
    (let ((run-context (make-instance 'my-run-context
                                      :merge-base nil
                                      :main-branch-hash "abcd"
                                      :env (make-env-reader)))
          (repo (make-instance 'git:null-repo))
          (calledp nil))
      (answer (run-context:git-repo run-context)
        repo)
      (if-called 'git:merge-base
                 (lambda (git one two)
                   (setf calledp t)
                   (error 'uiop:subprocess-error)))
      (finishes
        (run-context:merge-base run-context))
      (is-true calledp))))

(test fix-commit-hash
  (with-fixture state ()
    (let ((run-context (make-instance 'my-run-context))
          (repo :fake-repo))
      (answer (run-context:git-repo run-context) repo)
      (is (equal nil (fix-commit-hash run-context "")))
      (is (equal "ce04ccee65b8af8ac13a07c21f22b887dedd88f9" (fix-commit-hash run-context "ce04ccee65b8af8ac13a07c21f22b887dedd88f9")))
      (if-called 'git:rev-parse-local
                 (lambda (repo commit)
                   (is (eql :fake-repo repo))
                   (is (equal "ce044" commit))
                   "ce04ccee65b8af8ac13a07c21f22b887dedd88f9"))
      (is (equal "ce04ccee65b8af8ac13a07c21f22b887dedd88f9"
                 (fix-commit-hash run-context "ce044"))))))

(test fix-commit-hash-actually-fixes-commit-hashes
  (with-fixture state ()
    (test-git:with-git-repo (repo)
      (test-git:make-commit repo "foo")
      (let ((run-context (make-instance 'my-run-context)))
        (answer (run-context:git-repo run-context) repo)
        ;; a commit of length 40 doesn't change at all
        (let ((head (git:rev-parse-local repo "HEAD")))
          (assert-that head (has-length 40))
          (assert-that
           (fix-commit-hash run-context (str:substring 0 10 head))
           (is-equal-to head)))))))

(test pixel-tolerance-flag-propagated-to-flags-run-context
  "Test that the --pixel-tolerance flag is properly accessible in flags-run-context"
  (with-fixture state ()
    (let ((flags:*pixel-tolerance* 3))
      (let ((run-context (make-instance 'flags-run-context)))
        (assert-that (run-context:pixel-tolerance run-context)
                     (is-equal-to 3))))))
