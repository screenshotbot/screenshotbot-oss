;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-batch
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/batch
                #:batch-item
                #:batch)
  (:import-from #:screenshotbot/testing
                #:with-test-user
                #:with-installation
                #:screenshot-test)
  (:import-from #:screenshotbot/dashboard/batch
                #:accept-all
                #:reports-to-accept
                #:batch-item-link
                #:render-batch)
  (:import-from #:screenshotbot/model/report
                #:base-acceptable
                #:acceptable-state)
  (:import-from #:screenshotbot/report-api
                #:report-acceptable)
  (:import-from #:screenshotbot/model/channel
                #:review-policy-name)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/report-api
                #:report))
(in-package :screenshotbot/dashboard/test-batch)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-installation ()
     (with-fake-request ()
       (let* ((company (make-instance 'company))
              (batch (make-instance 'batch))
              (run (make-recorder-run))
              (report (make-instance 'report)))
         (&body))))))

(screenshot-test batch-item-empty-view
  (with-fixture state ()
    (render-batch batch)))

(screenshot-test batch-item-with-a-single-batches
  (with-fixture state ()
    (let ((channel (make-instance 'channel
                                  :company company
                                  :name "//foo:bar")))
      (make-instance 'batch-item
                     :run run
                     :status :accepted
                     :channel channel
                     :batch batch)
      (render-batch batch))))

(screenshot-test batch-item-with-few-batches
  (with-fixture state ()
    (flet ((mc (name)
             (make-instance 'channel
                            :company company
                            :name name)))
     (make-instance 'batch-item
                    :run run
                    :status :accepted
                    :channel (mc "//one")
                    :batch batch)
      (make-instance 'batch-item
                     :run run
                     :status :rejected
                     :channel (mc "//two")
                     :batch batch)
      (make-instance 'batch-item
                     :run run
                     :status :action-required
                     :channel (mc "//three")
                     :batch batch)
      (make-instance 'batch-item
                     :run run
                     :status :action-required
                     :channel (mc "//four")
                     :batch batch)
      (make-instance 'batch-item
                     :run run
                     :status :success
                     :channel (mc "//five")
                     :batch batch)
      (render-batch batch))))


(test batch-item-link
  (with-fixture state ()
    (is (not (str:emptyp
              (batch-item-link
               (make-instance 'batch-item
                              :run run)))))
    (is (not (str:emptyp
              (batch-item-link
               (make-instance 'batch-item
                              :run report)))))))

(def-fixture accept-all-state ()
  (with-test-store ()
    (with-test-user (:user user :company company :logged-in-p t)
      (let ((batch (make-instance 'batch :company company)))
        (flet ((make-item (name &key state (author "someone@example.com")
                                  (acceptable-p t))
                 (let* ((channel (make-instance 'channel
                                                :company company
                                                :name name))
                        (run (make-recorder-run :company company
                                                :channel channel
                                                :author author))
                        (report (make-instance 'report
                                               :run run
                                               :channel channel
                                               :title "1 change")))
                   (when acceptable-p
                     (setf (report-acceptable report)
                           (make-instance 'base-acceptable
                                          :report report
                                          :state state)))
                   (make-instance 'batch-item
                                  :report report
                                  :status :action-required
                                  :channel channel
                                  :batch batch)
                   report)))
          (&body))))))

(test accept-all-accepts-everything-not-yet-accepted
  (with-fixture accept-all-state ()
    (let ((pending (make-item "//pending"))
          (rejected (make-item "//rejected" :state :rejected))
          (accepted (make-item "//accepted" :state :accepted)))
      ;; A report without an acceptable has nothing to accept
      (make-item "//no-acceptable" :acceptable-p nil)
      (is (equal 2 (length (reports-to-accept batch user))))
      (accept-all batch user)
      (dolist (report (list pending rejected accepted))
        (is (eql :accepted (acceptable-state (report-acceptable report)))))
      (is (eql user (screenshotbot/model/report:acceptable-reviewer
                     (report-acceptable pending))))
      ;; The one that was already accepted wasn't touched
      (is (null (screenshotbot/model/report::acceptable-history
                 (report-acceptable accepted))))
      (is (null (reports-to-accept batch user))))))

(test accept-all-does-nothing-when-logged-out
  (with-fixture accept-all-state ()
    (make-item "//pending")
    (is (null (reports-to-accept batch nil)))))

(test accept-all-skips-reports-from-other-companies
  (with-fixture accept-all-state ()
    (make-item "//pending")
    (let ((outsider (screenshotbot/model/user:make-user
                     :companies (list (make-instance 'company)))))
      (is (null (reports-to-accept batch outsider))))))

(test accept-all-respects-the-review-policy
  (with-fixture accept-all-state ()
    (let ((mine (make-item "//mine" :author (auth:user-email user)))
          (theirs (make-item "//theirs")))
      (dolist (report (list mine theirs))
        (setf (review-policy-name (screenshotbot/model/report:report-channel report))
              :disallow-author))
      (accept-all batch user)
      (is (eql nil (acceptable-state (report-acceptable mine))))
      (is (eql :accepted (acceptable-state (report-acceptable theirs)))))))

(screenshot-test batch-with-accept-all-link
  (with-fixture accept-all-state ()
    (make-item "//pending")
    (render-batch batch)))
