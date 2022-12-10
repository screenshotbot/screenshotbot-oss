;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/slack/test-task-integration
  (:use #:cl
        #:fiveam)
  (:import-from #:cl-mock
                #:if-called)
  (:import-from #:screenshotbot/slack/core
                #:slack-error
                #:slack-token
                #:find-or-create-slack-config
                #:slack-post-on-channel)
  (:import-from #:screenshotbot/slack/task-integration
                #:slack-task-integration)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/report-api
                #:report)
  (:import-from #:screenshotbot/task-integration-api
                #:enabledp
                #:send-task)
  (:import-from #:screenshotbot/model/company
                #:default-slack-config
                #:company)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/user-api
                #:access-token)
  (:import-from #:screenshotbot/model/channel
                #:channel)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/slack/test-task-integration)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-installation ()
   (with-test-store ()
     (let ((posts))
       (cl-mock:with-mocks ()
         (if-called 'slack-post-on-channel
                    (lambda (&rest args)
                      (push args posts)))
         (let* ((company (make-instance 'company))
                (channel (make-instance 'channel :name "foobar"))
                (report (make-instance 'report :channel channel))
                (self (make-instance 'slack-task-integration
                                     :company company))
                (slack-token (make-instance 'slack-token
                                            :access-token "Foobar")))

           (let ((slack-config (find-or-create-slack-config company)))
             (with-transaction ()
               (setf (access-token slack-config) slack-token)
               (setf (default-slack-config company) slack-config)
               (setf (enabledp slack-config) t)))
           (&body)))))))

(test preconditions
  (with-fixture state ()
    (send-task self report)
    (pass)))

(test handles-slack-error
  (with-fixture state ()
    (let ((called nil))
      (if-called 'slack-post-on-channel
                  (lambda (&rest args)
                    (setf called t)
                    (error 'slack-error))
                  :at-start t)
      (send-task self report)
      (is-true called))))
