;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/test-slynk-preparer
  (:use #:cl
        #:fiveam)
  (:import-from #:cl-mock
                #:if-called
                #:answer
                #:with-mocks)
  (:import-from #:server
                #:*slynk-port*)
  (:import-from #:server/slynk-preparer
                #:no-available-port
                #:*actual-slynk-port*
                #:try-different-ports))
(in-package :server/test-slynk-preparer)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-mocks ()
    (let ((*actual-slynk-port* nil)
          (*slynk-port* "4001"))
      (&body))))

(test try-different-ports-happy-path
  (with-fixture state ()
    (answer (slynk:create-server
             :port 4001
             :dont-close t)
      t)
    (try-different-ports)
    (is (equal 4001 *actual-slynk-port*))))

(test try-different-ports-with-a-failure
  (with-fixture state ()
    (if-called 'slynk:create-server
               (lambda (&key port dont-close)
                 (cond
                   ((eql 4001 port)
                    (error "bad"))
                   (t
                    t))))
    (try-different-ports)
    (is (equal 4002 *actual-slynk-port*))))


(test try-all-ports-failed
  (with-fixture state ()
    (if-called 'slynk:create-server
               (lambda (&key port dont-close)
                 (error "bad")))
    (signals no-available-port
     (try-different-ports))))
