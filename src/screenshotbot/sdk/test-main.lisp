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
                #:warn-when-obsolete-flags
                #:*api-secret*
                #:*api-key*
                #:*hostname*
                #:make-api-context
                #:%main)
  (:import-from #:screenshotbot/sdk/env
                #:api-hostname
                #:make-env-reader)
  (:local-nicknames (#:a #:alexandria)
                    (#:flags #:screenshotbot/sdk/flags)
                    (#:sdk #:screenshotbot/sdk/sdk)
                    (#:static #:screenshotbot/sdk/static)
                    (#:api-context #:screenshotbot/sdk/api-context)
                    (#:firebase #:screenshotbot/sdk/firebase)))
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


(test simple-make-api-context
  (with-fixture state ()
    (let ((*hostname* "https://bleh.com")
          (*api-key* "foo")
          (*api-secret* "bleh"))
      (finishes
        (make-api-context)))))

(test simple-make-api-context-domain
  (with-fixture state ()
    (let ((*hostname* "bleh.com")
          (*api-key* "foo")
          (*api-secret* "bleh"))
      (let ((ctx
              (make-api-context)))
        (is (equal "https://bleh.com"
                   (api-context:hostname ctx)))))))

(test reads-hostname-from-env
  (with-fixture state ()
    (cl-mock:answer (make-env-reader) :env)
    (cl-mock:answer (api-hostname :env)
      "zoidberg.com")
    (let ((*api-key* "foo")
          (*api-secret* "bleh"))
      (let ((ctx
              (make-api-context)))
        (is (equal "https://zoidberg.com"
                   (api-context:hostname ctx)))))))

(test reads-hostname-from-env-2
  (with-fixture state ()
    (cl-mock:answer (make-env-reader) :env)
    (cl-mock:answer (api-hostname :env)
      "https://staging.screenshotbot.io")
    (let ((*hostname* "")
          (*api-key* "foo")
          (*api-secret* "bleh"))
      (let ((ctx
              (make-api-context)))
        (is (equal "https://staging.screenshotbot.io"
                   (api-context:hostname ctx)))))))



(test warn-when-obsolete-flags
  (with-fixture state ()
    (let ((saw nil))
      (handler-bind ((warning (lambda (w)
                                (setf saw w))))
        (warn-when-obsolete-flags))
      (is-false saw))
    (let ((flags:*ios-multi-dir* t))
      (signals simple-warning
       (warn-when-obsolete-flags)))))
