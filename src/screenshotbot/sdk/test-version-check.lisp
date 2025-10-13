;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-version-check
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/version-check
                #:bad-version-p
                #:warn-for-bad-versions)
  (:import-from #:util/request
                #:*engine*
                #:http-request)
  (:import-from #:cl-mock
                #:if-called
                #:answer)
  (:import-from #:screenshotbot/sdk/api-context
                #:fetch-version
                #:remote-version
                #:api-context)
  (:import-from #:screenshotbot/api/model
                #:*api-version*)
  (:local-nicknames (#:a #:alexandria)
                    (#:flags #:screenshotbot/sdk/flags)))
(in-package :screenshotbot/sdk/test-version-check)


(util/fiveam:def-suite)

(def-fixture state ()
  (let ((auto-restart:*global-enable-auto-retries-p* nil))
    (cl-mock:with-mocks ()
      (&body))))

(test get-version
  (with-fixture state ()
    (answer (http-request "https://api.screenshotbot.io/api/version"
                          :basic-authorization (list "" "")
                          :want-string t
                          :engine *engine*)
      (values "{\"version\":1}" 200))
    (is (eql 1 (fetch-version
                (make-instance 'api-context
                               :key ""
                               :secret ""
                               :hostname "https://api.screenshotbot.io"))))))

#-sbcl ;; See D7222. Temporary fix until we can see what's going on with this.
(test get-version-404
  (with-fixture state ()
    (answer (http-request "https://www.google.com/api/version"
                          :basic-authorization (list "" "")                          
                          :want-string t
                          :engine *engine*)
      (values "" 404))
    (is (eql 1 (fetch-version
                (make-instance 'api-context
                               :key ""
                               :secret ""
                               :hostname "https://www.google.com"))))))

(test with-version-check
  (with-fixture state ()
    (if-called 'fetch-version
               (lambda (api-context)
                 189))
    (let ((ans))
      (let ((api-context (make-instance 'api-context
                                        :hostname "...")))
        (is (eql 189 (remote-version api-context)))
        (is (eql 189 (remote-version api-context)))))))

(test bad-version-p ()
  (let ((*api-version* 102))
    (is-true (bad-version-p 99))
    (is-true (bad-version-p 107))
    (is-false (bad-version-p 102))
    (is-false (bad-version-p 101))
    (is-false (bad-version-p 103))))

(test warn-for-bad-versions ()
  (let ((*api-version* 102))
    (finishes (warn-for-bad-versions 102))
    (finishes (warn-for-bad-versions 95))
    (finishes (warn-for-bad-versions 106))))



