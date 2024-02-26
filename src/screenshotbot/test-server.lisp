;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-server
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/server
                #:request)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-server)


(util/fiveam:def-suite)

(test authenticate-request-happy-path
  (with-installation ()
   (with-test-store ()
     (with-fake-request ()
       (let ((request (make-instance 'request
                                     :uri "/foo"
                                     :headers-in nil)))
         (auth:with-sessions ()
           (finishes
            (auth:authenticate-request request))))))))
