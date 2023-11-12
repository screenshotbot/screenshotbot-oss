;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/dashboard/test-api-keys
    (:use #:cl
          #:alexandria
          #:fiveam
          #:screenshotbot/api-key-api)
  (:import-from #:screenshotbot/dashboard/api-keys
                #:api-key-cli-generate
                #:with-description
                #:%api-key-page)
  (:import-from #:screenshotbot/factory
                #:test-user
                #:test-api-key
                #:*company*)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*)
  (:import-from #:screenshotbot/template
                #:*template-override*)
  (:import-from #:util/testing
                #:with-fake-request
                #:screenshot-static-page)
  (:import-from #:screenshotbot/testing
                #:with-test-user
                #:with-installation
                #:screenshot-test)
  (:import-from #:cl-mock
                #:if-called
                #:answer)
  (:import-from #:screenshotbot/model/api-key
                #:render-api-token)
  (:import-from #:util/store/store
                #:with-test-store))

(util/fiveam:def-suite)

(markup:enable-reader)


(test simple-page-test
  (let* ((*installation* (make-instance 'installation)))
    (with-fake-request ()
      (auth:with-sessions ()
       (screenshot-static-page
        :screenshotbot
        "api-key-page"
        (markup:write-html
         (%api-key-page :user (make-instance 'test-user
                                             :api-keys (list (make-instance 'test-api-key
                                                                            :key "foo"
                                                                            :secret "sdfsdfdfdfs")))
                        :company *company*)))))))

(test empty-api-keys-page-test
  (let* ((*installation* (make-instance 'installation)))
    (with-fake-request ()
      (auth:with-sessions ()
       (screenshot-static-page
        :screenshotbot
        "api-key-page-empty"
        (markup:write-html
         (%api-key-page :user (make-instance 'test-user)
                        :company *company*)))))))

(screenshot-test api-key-page-description-page
  (with-installation ()
   (with-fake-request ()
     (auth:with-sessions ()
       (with-description (description)
         (values))))))

(screenshot-test cli-generation-page
  (with-installation ()
    (with-test-store ()
     (with-fake-request ()
       (with-test-user (:logged-in-p t)
         (auth:with-sessions ()
           (cl-mock:with-mocks ()
             (if-called 'render-api-token
                        (lambda (key)
                          "cli-0000:xyzd"))
             (api-key-cli-generate))))))))
