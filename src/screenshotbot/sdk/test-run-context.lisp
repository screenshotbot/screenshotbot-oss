;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-run-context
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/run-context
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
  (:local-nicknames (#:run-context #:screenshotbot/sdk/run-context)
                    (#:git #:screenshotbot/sdk/git)
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
      (is (equal nil
                 (run-context:override-commit-hash
                  (make-instance 'test-run-context
                                 :env (make-env-reader))))))))


(test parse-override-commit-hash-with-bitrise
  (with-fixture state ()
    (with-env ((:bitrise-git-commit "abcd")
               (:bitriseio-pull-request-repository-url "https://bitbucket.org/tdrhq/fast-example"))
      (is (equal nil (run-context:pull-request-url
                      (make-instance 'test-run-context))))
      (is (equal nil (run-context:override-commit-hash
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
