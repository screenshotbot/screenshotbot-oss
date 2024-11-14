;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(defpackage :screenshotbot/azure/test-request
  (:use #:cl
        #:fiveam)
  (:import-from #:util/request
                #:*engine*
                #:http-request-impl)
  (:import-from #:screenshotbot/azure/request
                #:azure-unauthorized-error
                #:azure-request
                #:azure))
(in-package :screenshotbot/azure/test-request)


(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
    ;; if you need a non-401-engine, still keep this around as a
    ;; default for these dtests.
    (let ((*engine* (make-instance '401-engine))
          (azure (make-instance 'azure
                                :token "dfd"
                                :organization "testsbot"
                                :project "fast-example")))
     (&body))))

(defclass 401-engine ()
  ())

(defmethod http-request-impl ((self 401-engine)
                              url &key &allow-other-keys)
  (values "" 401 nil))

(test handles-401-more-gracefully
  (with-fixture state ()
    (signals azure-unauthorized-error
      (azure-request
       azure
       "foo/bar"
       :method :post))))

