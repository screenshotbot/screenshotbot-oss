;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-main
  (:use #:cl
        #:fiveam)
  (:import-from #:cl-mock
                #:with-mocks)
  (:import-from #:screenshotbot/sdk/main
                #:%main))
(in-package :screenshotbot/sdk/test-main)


(util/fiveam:def-suite)

(define-condition quit-condition (error)
  ((code :initarg :code)))

(define-condition success-condition (error)
  ())

(def-fixture state ()
  (with-mocks ()
    (cl-mock:if-called 'log:config
                       (lambda (&rest args)
                         (declare (ignore args))))
    (cl-mock:if-called 'uiop:quit
                       (lambda (code)
                         (case code
                           (0
                            (error 'success-condition))
                           (otherwise
                            (error 'quit-condition :code code)))))
    (&body)))

(test simple-parsing
  (with-fixture state ()
    (finishes
      (%main (list "./recorder" "--help")))))

(test unrecognized-command
  (with-fixture state ()
    (signals quit-condition
      (%main (list "./recorder" "--helpy")))))
