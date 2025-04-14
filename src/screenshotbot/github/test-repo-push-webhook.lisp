;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/github/test-repo-push-webhook
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/request
                #:http-request
                #:*engine*)
  (:import-from #:util/hunchentoot-engine
                #:hunchentoot-engine)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:screenshotbot/server
                #:*acceptor*)
  (:import-from #:screenshotbot/testing
                #:with-installation))
(in-package :screenshotbot/github/test-repo-push-webhook)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-installation ()
   (let ((*engine* (make-instance 'hunchentoot-engine
                                  :acceptor *acceptor*)))
     (with-test-store ()
       (let ((company (make-instance 'company)))
         (&body))))))

(test simple-request
  (with-fixture state ()
    (is (eql 0 (store-object-id company)))
    (http-request
     "https://localhost/github/0/push/tdrhq/fast-example"
     :method :post
     :content "{}"
     :additional-headers `((:x-hub-signature-256 . "sha256=004835e2ffdb07054a2a5fa4137321ce04474fc9ef48d75a7de6711dd5820fc5")))))


