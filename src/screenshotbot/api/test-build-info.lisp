;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/test-build-info
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/api/build-info
                #:%get-build-info
                #:%post-build-info
                #:to-dto)
  (:import-from #:screenshotbot/model/build-info
                #:build-info
                #:find-build-info
                #:find-or-create-build-info
                #:build-url
                #:build-info-repo-url)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:with-test-user
                #:with-installation
                #:screenshot-test)
  (:import-from #:screenshotbot/api/model
                #:build-info-build-url)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/test-build-info)

(util/fiveam:def-suite)


(def-fixture state ()
  (cl-mock:with-mocks ()
   (with-installation ()
     (with-test-store ()
       (with-test-user (:company company :logged-in-p t)
         (auth:with-sessions ()
           (&body)))))))

(test to-dto-conversion
  (with-fixture state ()
    (let ((build-info (make-instance 'build-info
                                     :build-url "https://ci.example.com/build/123"
                                     :company company
                                     :repo-url "https://github.com/example/repo")))
      (let ((dto (to-dto build-info)))
        (is (string= "https://ci.example.com/build/123" (dto:build-info-build-url dto)))
        (is (string= "https://github.com/example/repo" (dto:build-info-repo-url dto)))))))

(test get-build-info-existing
  (with-fixture state ()
    (let ((build-info (find-or-create-build-info company "https://ci.example.com/build/456")))
      (setf (build-info-repo-url build-info) "https://github.com/example/repo2")
      (let ((result (%get-build-info :build-url "https://ci.example.com/build/456")))
        (is (not (null result)))
        (is (string= "https://ci.example.com/build/456" (dto:build-info-build-url result)))
        (is (string= "https://github.com/example/repo2" (dto:build-info-repo-url result)))))))

(test get-build-info-non-existing
  (with-fixture state ()
    (let ((result (%get-build-info :build-url "https://ci.example.com/nonexistent")))
      (is (null result)))))

(test post-build-info-create-new
  (with-fixture state ()
    (let ((input (make-instance 'dto:build-info
                                :build-url "https://ci.example.com/build/789"
                                :repo-url "https://github.com/example/repo3")))
      (cl-mock:if-called 'hunchentoot:raw-post-data
                         (lambda (&rest args)
                           (dto:encode-json input)))
      (let ((result (%post-build-info)))
        (is (not (null result)))
        (is (string= "https://ci.example.com/build/789" (build-info-build-url result)))
        (is (string= "https://github.com/example/repo3" (dto:build-info-repo-url result)))
        (let ((stored (find-build-info company "https://ci.example.com/build/789")))
          (is (not (null stored)))
          (is (string= "https://github.com/example/repo3" (build-info-repo-url stored))))))))

(test post-build-info-update-existing
  (with-fixture state ()
    (let ((existing (find-or-create-build-info company "https://ci.example.com/build/update")))
      (setf (build-info-repo-url existing) "https://github.com/example/old-repo")
      (let ((input (make-instance 'dto:build-info
                                  :build-url "https://ci.example.com/build/update"
                                  :repo-url "https://github.com/example/new-repo")))
        (cl-mock:if-called 'hunchentoot:raw-post-data
                           (lambda (&rest args)
                             (dto:encode-json input)))
        (let ((result (%post-build-info)))
          (is (not (null result)))
          (is (string= "https://ci.example.com/build/update" (build-info-build-url result)))
          (is (string= "https://github.com/example/new-repo" (dto:build-info-repo-url result)))
          (is (string= "https://github.com/example/new-repo" (build-info-repo-url existing))))))))


