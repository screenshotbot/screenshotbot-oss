;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/gitlab/test-repo
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/gitlab/repo
                #:*public-repo-p-cache*
                #:cached-public-repo-p
                #:gitlab-repo)
  (:import-from #:cl-mock
                #:if-called))
(in-package :screenshotbot/gitlab/test-repo)

(util/fiveam:def-suite)

(def-fixture state (&key (response (lambda () "{}")))
  (cl-mock:with-mocks ()
    (let ((*public-repo-p-cache* (make-hash-table :test #'equal))
          (calls 0)
          (repo (make-instance 'gitlab-repo
                               :link "https://gitlab.com/tdrhq/bar.git")))
      (if-called 'dex:get
                 (lambda (url &rest args)
                   (declare (ignore url args))
                   (incf calls)
                   (funcall response)))
      (&body))))

(test public-repo-p-is-cached
  (with-fixture state ()
    (is-true (cached-public-repo-p repo :now 1000))
    (is-true (cached-public-repo-p repo :now 1010))
    (is (eql 1 calls))))

(test cache-is-shared-across-repo-objects
  (with-fixture state ()
    (is-true (cached-public-repo-p repo :now 1000))
    (is-true (cached-public-repo-p
              (make-instance 'gitlab-repo
                             :link "https://gitlab.com/tdrhq/bar.git")
              :now 1010))
    (is (eql 1 calls))))

(test cache-expires
  (with-fixture state ()
    (is-true (cached-public-repo-p repo :now 1000))
    (is-true (cached-public-repo-p repo :now 5000))
    (is (eql 2 calls))))

(test not-found-is-cached-as-private
  (with-fixture state (:response
                       (lambda ()
                         (error 'dexador.error:http-request-not-found
                                :status 404
                                :body ""
                                :uri "https://gitlab.com"
                                :headers (make-hash-table)
                                :method :get)))
    (is-false (cached-public-repo-p repo :now 1000))
    (is-false (cached-public-repo-p repo :now 1010))
    (is (eql 1 calls))))
