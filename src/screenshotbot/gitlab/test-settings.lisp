;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/gitlab/test-settings
  (:use #:cl
        #:fiveam
        #:fiveam-matchers)
  (:import-from #:screenshotbot/gitlab/repo
                #:gitlab-repo)
  (:import-from #:screenshotbot/gitlab/plugin
                #:gitlab-plugin)
  (:import-from #:screenshotbot/plugin
                #:plugin-parse-repo)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/gitlab/settings
                #:gitlab-request
                #:gitlab-url
                #:gitlab-token
                #:save-settings
                #:settings-page
                #:gitlab-settings)
  (:import-from #:screenshotbot/testing
                #:with-test-user
                #:with-installation
                #:screenshot-test)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:alexandria
                #:assoc-value))
(in-package :screenshotbot/gitlab/test-settings)


(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
   (with-installation ()
     (with-test-store ()
       (let* ((company (make-instance 'company))
              (plugin (make-instance 'gitlab-plugin)))
         (&body))))))

(test gitlab-plugin-parse-repo
  (with-fixture state ()
    (let ((settings (make-instance 'gitlab-settings
                                   :company company
                                   :url "https://gitlab.com")))
     (assert-that
      (plugin-parse-repo plugin
                         company
                         "https://gitlab.com/tdrhq/fast-example.git")
      (has-typep 'gitlab-repo)))))

(screenshot-test gitlab-settings-page
  (with-fixture state ()
    (with-fake-request ()
      (auth:with-sessions ()
        (settings-page)))))

(test save-settings
  (with-fixture state ()
    (with-test-user (:user user :company company :logged-in-p t)
      (setf (roles:user-role company user) 'roles:admin)
      (let ((settings (make-instance 'gitlab-settings
                                     :company company)))
        (signals hex:redirected
          (save-settings "https://gitlab.example.com"
                         "test-token"
                         ""))
        (assert-that (gitlab-url settings)
                     (equal-to "https://gitlab.example.com"))
        (assert-that (gitlab-token settings)
                     (equal-to "test-token"))))))

(screenshot-test save-settings-non-admin
  (with-fixture state ()
    (with-test-user (:user user :company company :logged-in-p t)
      (setf (roles:user-role user company) 'roles:standard-member)
      (let ((settings (make-instance 'gitlab-settings
                                     :company company
                                     :url "https://example.com"
                                     :token "foobar")))
        (prog1
            (save-settings "https://gitlab.example.com"
                        "test-token"
                        "")
          (assert-that (gitlab-url settings)
                       (equal-to "https://example.com"))
          (assert-that (gitlab-token settings)
                       (equal-to "foobar")))))))

(test gitlab-request-uses-token
  (with-fixture state ()
    (let ((saved-additional-headers))
     (with-test-user (:company company)
       (cl-mock:if-called
        'util/request:http-request
        (lambda (url &key additional-headers &allow-other-keys)
          (setf saved-additional-headers additional-headers)))
       (gitlab-request company
                       "/api/foo"
                       :token "fOObARDummyToken"
                       :gitlab-url "https://example.com")
       (is
        (equal "fOObARDummyToken"
               (assoc-value saved-additional-headers
                            "PRIVATE-TOKEN"
                            :test #'equal)))))))
