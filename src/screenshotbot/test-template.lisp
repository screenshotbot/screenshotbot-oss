;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/test-template
    (:use #:cl
          #:alexandria
          #:./template
          #:fiveam)
  (:import-from #:./factory
                #:*user*
                #:*company*)
  (:import-from #:screenshotbot/template
                #:something-went-wrong)
  (:import-from #:fiveam-matchers
                #:is-string
                #:has-typep
                #:assert-that)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*))

(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*installation* (make-instance 'installation)))
    (&body)))

(test simple-template
  (with-fixture state ()
   (screenshotbot/template:dashboard-template
    :user *user*
    :company *company*
    :script-name "/runs"))
  (pass))


(test landing-template
  (with-fixture state ()
   (landing-template
    "foo"))
  (pass))

(test something-went-wrong
  (with-fixture state ()
   (assert-that
    (something-went-wrong)
    (is-string))))
