;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-api-keys
  (:use :cl)
  (:import-from #:cl-mock
                #:if-called)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:core/ui/template
                #:*app-template*)
  (:import-from #:it.bese.fiveam
                #:def-fixture
                #:test
                #:with-fixture)
  (:import-from #:screenshotbot/dashboard/api-keys
                #:%api-key-page
                #:api-key-cli-generate
                #:with-description)
  (:import-from #:screenshotbot/factory
                #:*company*
                #:test-api-key
                #:test-user)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:screenshotbot/model/api-key
                #:render-api-token)
  (:import-from #:screenshotbot/server
                #:screenshotbot-template)
  (:import-from #:screenshotbot/testing
                #:screenshot-test
                #:with-installation
                #:with-test-user)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:test-acceptor
                #:screenshot-static-page
                #:with-fake-request)
  (:import-from #:core/api/acceptor
                #:api-acceptor-mixin))
(in-package :screenshotbot/dashboard/test-api-keys)


(util/fiveam:def-suite)

(defclass my-acceptor (api-acceptor-mixin
                       test-acceptor)
  ())

(util/fiveam:def-suite)

(markup:enable-reader)

(def-fixture state ()
  (let* ((*installation* (make-instance 'installation))
         (*app-template* (make-instance 'screenshotbot-template)))
    (&body)))


(test simple-page-test
  (with-fixture state ()
    (with-fake-request (:acceptor 'my-acceptor)
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
  (with-fixture state ()
    (with-fake-request (:acceptor 'my-acceptor)
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
