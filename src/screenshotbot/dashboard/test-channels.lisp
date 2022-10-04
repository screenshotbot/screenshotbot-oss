;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/dashboard/test-channels
    (:use #:cl
          #:alexandria
          #:screenshotbot/user-api
          #:fiveam)
  (:import-from #:screenshotbot/dashboard/channels
                #:%list-projects)
  (:import-from #:screenshotbot/factory
                #:test-user
                #:test-channel
                #:test-company)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*))

(util/fiveam:def-suite)

(defclass company-with-channels (test-company)
  ())

(defmethod company-channels ((company company-with-channels))
  (list (make-instance 'test-channel)))

(test simple-view
  (let ((*installation* (make-instance 'installation)))
   (let ((user (make-instance 'test-user))
         (company (make-instance 'company-with-channels)))
     (%list-projects :user user
                     :company company))))
