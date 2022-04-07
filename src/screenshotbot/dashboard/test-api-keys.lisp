;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/dashboard/test-api-keys
    (:use #:cl
          #:alexandria
          #:fiveam
          #:../api-key-api)
  (:import-from #:./api-keys
                #:%api-key-page)
  (:import-from #:../factory
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
                #:screenshot-static-page))

(util/fiveam:def-suite)

(markup:enable-reader)

(defun dummy-template (children)
  <div>
    ,@children
  </div>)

(test simple-page-test
  (let* ((*installation* (make-instance 'installation))
         (*template-override* #'dummy-template))
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
