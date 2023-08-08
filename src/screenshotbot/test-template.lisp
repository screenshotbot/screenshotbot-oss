;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/test-template
    (:use #:cl
          #:alexandria
          #:screenshotbot/template
          #:fiveam)
  (:import-from #:screenshotbot/factory
                #:*user*
                #:*company*)
  (:import-from #:screenshotbot/template
                #:analyticsp
                #:something-went-wrong)
  (:import-from #:fiveam-matchers
                #:is-string
                #:has-typep
                #:assert-that)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*)
  (:import-from #:screenshotbot/server
                #:no-access-error-page
                #:acceptor)
  (:import-from #:util/testing
                #:with-fake-request
                #:screenshot-static-page)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:screenshot-test))

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

(test screenshot-404-page
  (with-fixture state ()
    (screenshot-static-page
      :screenshotbot
      "404-page"
      (hunchentoot:acceptor-status-message
       (make-instance 'acceptor)
       404))))

(screenshot-test no-access-error-page
  (with-fixture state ()
    (with-fake-request ()
     (auth:with-sessions ()
       (no-access-error-page)))))

(defclass my-installation (installation)
  ((analyticsp :initform t
               :reader analyticsp)))

#-screenshotbot-oss
(test analytics
  (with-installation (:installation (make-instance 'my-installation :domain "https://foo.screenshotbot.io"))
    (with-fake-request ()
     (let ((tag (google-analytics)))
       (is-true tag)
       (is (equal "foo.screenshotbot.io"
                  (mquery:attr tag "data-domain")))))))
