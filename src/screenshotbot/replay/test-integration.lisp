;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/test-integration
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/replay/integration
                #:get-local-addr
                #:replay-job-from-snapshot)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/replay/browser-config
                #:browser-config)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/replay/core
                #:asset
                #:snapshot)
  (:import-from #:screenshotbot/webdriver/impl
                #:call-with-webdriver)
  (:import-from #:screenshotbot/replay/replay-acceptor
                #:*default-render-acceptor*)
  (:local-nicknames (#:a #:alexandria)
                    (#:integration #:screenshotbot/replay/integration)))
(in-package :screenshotbot/replay/test-integration)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (tmpdir:with-tmpdir (tmpdir)
      (cl-mock:with-mocks ()
        (cl-mock:if-called 'get-local-addr
                            (lambda (&rest args)
                              "9.9.9.9"))
        (cl-mock:if-called 'hunchentoot:start
                            (lambda (acceptor) (declare (ignore acceptor))))
        (let* ((company (make-instance 'company))
               (*default-render-acceptor* nil)
               (asset (make-instance 'asset
                                      :file "foo"))
               (snapshot (make-instance 'snapshot
                                         :assets (list asset)
                                         :root-files (list "foo")))
               (run (make-instance 'integration:run
                                    :company company
                                    :browser-configs
                                    (list (make-instance 'browser-config
                                                          :type "chrome")))))
          (&body))))))

(test replay-job-from-snapshot-2
  (with-fixture state ()
    (cl-mock:if-called 'call-with-webdriver
                        (lambda (&rest args)
                          (declare (ignore args))))
    (replay-job-from-snapshot
     :run run
     :snapshot snapshot
     :urls (list "https://www.google.com")
     :tmpdir tmpdir)
    (pass)))
