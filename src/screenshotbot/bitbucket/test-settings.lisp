;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/pro/bitbucket/test-settings
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/pro/bitbucket/settings
                #:update-from-refresh-token
                #:refresh-token
                #:bitbucket-settings-for-company)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/pro/bitbucket/test-settings)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
   (let ((company (make-instance 'company)))
     (&body))))

(test preconditions
  (with-fixture state ()
    (update-from-refresh-token company
                               '((:refresh--token . "foo")))
    (is (equal 1 (length (bitbucket-settings-for-company company))))
    (is (equal
         "foo"
         (refresh-token (car (bitbucket-settings-for-company company)))))))

(test updating-refresh-token
  (with-fixture state ()
    (update-from-refresh-token company '((:refresh--token . "foo")))
    (update-from-refresh-token company '((:refresh--token . "bar")))
    (is (equal 2 (length (bitbucket-settings-for-company company))))
    (is (equal
         "bar"
         (refresh-token (car (bitbucket-settings-for-company company)))))))

(test when-refresh-token-doesnt-change
  (with-fixture state ()
    (update-from-refresh-token company '((:refresh--token . "foo")))
    (update-from-refresh-token company '((:refresh--token . "foo")))
    (is (equal 1 (length (bitbucket-settings-for-company company))))
    (is (equal
         "foo"
         (refresh-token (car (bitbucket-settings-for-company company)))))))
