;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/integration-fixture
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/user-api
                #:user)
  (:import-from #:screenshotbot/sdk/api-context
                #:api-context)
  (:import-from #:screenshotbot/server
                #:*acceptor*)
  (:import-from #:util/hunchentoot-engine
                #:hunchentoot-engine)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags)
                    (#:run-context #:screenshotbot/sdk/run-context)
                    (#:a #:alexandria)
                    (#:dto #:screenshotbot/api/model)
                    (#:api-key #:core/api/model/api-key)))
(in-package :screenshotbot/sdk/integration-fixture)

(defclass fake-api-context (api-context)
  ()
  (:default-initargs :engine (make-instance 'hunchentoot-engine
                                            :acceptor *acceptor*)))


(def-easy-macro with-sdk-integration (&binding api-context &key &binding company &fn fn)
  (with-installation ()
   (with-test-store (:globally t)
     (let* ((company (make-instance 'company))
            (user (make-instance 'user))
            (api-key (make-instance 'api-key:api-key
                                    :user user
                                    :company company))
            (api-context (make-instance 'fake-api-context
                                        :key (api-key:api-key-key api-key)
                                        :hostname "localhost"
                                        :secret (api-key:api-key-secret-key api-key))))
       (let ((*wrap-internal-errors* nil))
        (fn api-context company))))))




