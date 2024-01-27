;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-raft-state-http
  (:use #:cl
        #:fiveam)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:util/store/raft-state-http
                #:%response
                #:raft-state-request-p)
  (:import-from #:cl-mock
                #:answer)
  (:import-from #:bknr.cluster/server
                #:leaderp))
(in-package :util/store/test-raft-state-http)

(util/fiveam:def-suite)

(test raft-state-request-p
  (with-fake-request ()
    (is-true (raft-state-request-p
              (make-instance 'hunchentoot:request :uri "/raft-state/foobar"
                             :headers-in nil)))
    (is-false (raft-state-request-p
               (make-instance 'hunchentoot:request :uri "/sdfsdfds"
                                                   :headers-in nil)))))

(test response-happy-path
  (cl-mock:with-mocks ()
    (let ((bknr.datastore:*store* 'my-store))
      (answer (leaderp 'my-store) t)
      (is (equal "leader" (%response)))))
  (cl-mock:with-mocks ()
    (with-fake-request ()
     (let ((bknr.datastore:*store* 'my-store))
       (answer (leaderp 'my-store) nil)
       (is (equal "other" (%response)))
       (is (eql 400 (hunchentoot:return-code hunchentoot:*reply*)))))))
