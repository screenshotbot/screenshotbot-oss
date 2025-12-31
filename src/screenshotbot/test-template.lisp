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
                #:screenshot-test)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/user-api
                #:user)
  (:import-from #:core/installation/installation
                #:site-alert))

(util/fiveam:def-suite)

(named-readtables:in-readtable markup:syntax)

(def-fixture state ()
  (with-test-store ()
    (let ((*installation* (make-instance 'installation)))
      (let ((company (make-instance 'company :name "Test Company"))
            (user (make-instance 'user)))
        (&body)))))

(test simple-template
  (with-fixture state ()
   (screenshotbot/template:dashboard-template
    :user user
    :company company
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

(defclass test-installation-with-alert (installation)
  ())

(defmethod site-alert ((self test-installation-with-alert))
  <div class= "alert alert-warning" >
    <strong>Scheduled Maintenance:</strong> The system will undergo maintenance on December 31st from 2:00 AM to 4:00 AM EST.
  </div>)

(screenshot-test dashboard-with-site-alert
  (with-test-store ()
    (let ((*installation* (make-instance 'test-installation-with-alert)))
      (let ((company (make-instance 'company :name "Test Company"))
            (user (make-instance 'user)))
        (with-fake-request ()
          <dashboard-template user=user company=company script-name= "/runs">
            hello world
          </dashboard-template>)))))


