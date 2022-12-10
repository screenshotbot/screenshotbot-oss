;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-notices
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:with-test-user)
  (:import-from #:screenshotbot/user-api
                #:unaccepted-invites)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/model/invite
                #:invite)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:util/testing
                #:screenshot-static-page)
  (:import-from #:screenshotbot/template
                #:user-notice-list
                #:dashboard-template)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/dashboard/test-notices)


(util/fiveam:def-suite)

(test notices-screenshot-test
  (with-installation ()
   (with-test-store ()
     (with-test-user (:user user
                      :company company
                      :logged-in-p t)
       (with-transaction ()
         (setf (unaccepted-invites user)
               (list
                (make-instance 'invite
                               :code "dfd"
                               :used-p nil
                               :company company))))
       (screenshot-static-page
        :screenshotbot
        "notices"
        (dashboard-template))))))
