;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-run-context
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/run-context
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
                #:if-called)
  (:import-from #:fiveam-matchers/core
                #:error-with-string-matching
                #:signals-error-matching
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex
                #:contains-string)
  (:local-nicknames (#:run-context #:screenshotbot/sdk/run-context)
                    (#:git #:screenshotbot/sdk/git)
                    (#:dto #:screenshotbot/api/model)
                    (#:flags #:screenshotbot/sdk/flags)))
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
    (with-env ((:circle-sha1 "abcd")
               (:circle-pull-request "http://foo"))
      (is (equal "abcd"
                 (run-context:override-commit-hash
                  (make-instance 'test-run-context
                                 :env (make-env-reader)))))))
  (with-fixture state ()
    (with-env ((:circle-sha1 "abcd")
               (:circle-pull-request ""))
      (is (equal "abcd"
                 (run-context:override-commit-hash
                  (make-instance 'test-run-context
                                 :env (make-env-reader))))))))


(test parse-override-commit-hash-with-bitrise
  (with-fixture state ()
    (with-env ((:bitrise-git-commit "abcd")
               (:bitriseio-pull-request-repository-url "https://bitbucket.org/tdrhq/fast-example"))
      (is (equal nil (run-context:pull-request-url
                      (make-instance 'test-run-context))))
      (is (equal "abcd" (run-context:override-commit-hash
                         (make-instance 'test-run-context))))))

  (with-fixture state ()
    (with-env ((:bitrise-git-commit "abcd")
               (:bitrise-pull-request "1")
               (:bitriseio-pull-request-repository-url "https://bitbucket.org/tdrhq/fast-example"))

      (is (equal "https://bitbucket.org/tdrhq/fast-example/pull-requests/1"
                 (run-context:pull-request-url
                  (make-instance 'test-run-context))))

      (is (equal "abcd"
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
       (has-length 1))
      #+linux
      (assert-that
       (dto:metadata-value (first metadata))
       (contains-string "Linux"))
      #+darwin
      (assert-that
       (dto:metadata-value (first metadata))
       (contains-string "Darwin")))))

(test uname-should-not-crash
  (with-fixture state ()
    (let ((run-context (make-instance 'test-run-context)))
      (if-called 'uiop:run-program
                 (lambda (&rest args)
                   (error "uname crashed!")))
      (assert-that
       (run-context-metadata run-context)
       (has-length 1)))))

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




