;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-report
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/report-api
                #:report)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/screenshot-api
                #:make-screenshot)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/report
                #:base-acceptable
                #:report-to-dto
                #:company-promotion-reports
                #:%report-company)
  (:import-from #:screenshotbot/api/model
                #:encode-json)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:with-test-user)
  (:import-from #:screenshotbot/user-api
                #:channel
                #:can-view)
  (:import-from #:fiveam-matchers/lists
                #:has-item)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:screenshotbot/installation
                #:installation
                #:multi-org-feature)
  (:import-from #:auth/viewer-context
                #:api-viewer-context
                #:normal-viewer-context)
  (:import-from #:screenshotbot/model/api-key
                #:api-key)
  (:import-from #:screenshotbot/user-api
                #:user)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/model/test-report)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let* ((company (make-instance 'company))
           (run (make-instance 'recorder-run
                               :company company
                               :screenshot-map nil)))
      (&body))))

(test simple-creation ()
  (with-fixture state ()
    (finishes
      (make-instance 'report :acceptable nil))))

(test crashes-on-bad-args ()
  (with-fixture state ()
    (signals #+lispworks conditions:unknown-keyword-error
      #-lispworks error
      (make-instance 'report :does-not-exist-arg t))))

(test sets-company ()
  (with-fixture state ()
    (let ((report (make-instance 'report :run run)))
      (is (eql (%report-company report)
               company)))))

(test company-promoted-index
  (with-fixture state ()
    (let ((report-1 (make-instance 'report :run run
                                           :promotion-report-p t))
          (report-2 (make-instance 'report :run run)))
      (is
       (fset:equal?
        (fset:with (fset:empty-set) report-1)
        (company-promotion-reports company))))))

(test dto-is-serializable
  (with-fixture state ()
    (let ((report (make-instance 'report
                                 :run run
                                 :previous-run run)))
      (finishes
        (encode-json (report-to-dto report))))))

(defclass multi-install (multi-org-feature
                         installation)
  ())

(test can-view-on-report
  (with-fixture state ()
    (with-installation (:installation (make-instance 'multi-install))
     (with-test-user (:user user :company company)
       (let* ((channel (make-instance 'channel :company company))
              (run1 (make-recorder-run :company company
                                       :channel channel))
              (run2 (make-recorder-run :company company
                                       :channel channel))
              (report (make-instance 'report
                                     :run run1
                                     :previous-run run2)))
         (assert-that (roles:companies-for-user user)
                      (has-item company))
         (is-true user)
         (is-true (auth:can-viewer-view
                   (make-instance 'normal-viewer-context
                                  :user user)
                   report)))))))

(test can-view-on-report-with-nil-previous
  (with-fixture state ()
    (with-installation (:installation (make-instance 'multi-install))
     (with-test-user (:user user :company company)
       (let* ((channel (make-instance 'channel :company company))
              (run1 (make-recorder-run :company company
                                       :channel channel))
              (run2 nil)
              (report (make-instance 'report
                                     :run run1
                                     :previous-run run2)))
         (assert-that (roles:companies-for-user user)
                      (has-item company))
         (is-true user)
         (is-true (auth:can-viewer-view
                   (make-instance 'normal-viewer-context
                                  :user user)
                   report)))))))


(test review-state-encoding-without-acceptable
  (with-fixture state ()
    (let ((report (make-instance 'report :acceptable nil)))
      (is (equal "na" (dto:report-acceptable-state
                       (report-to-dto report)))))))

(test review-state-encoding-with-acceptable
  (with-fixture state ()
    (let* ((acceptable (make-instance 'base-acceptable))
           (report (make-instance 'report :acceptable acceptable)))
      (is (equal "none" (dto:report-acceptable-state
                         (report-to-dto report)))))))

(test review-state-encoding-with-acceptable
  (with-fixture state ()
    (let* ((acceptable (make-instance 'base-acceptable
                                      :state :accepted))
           (report (make-instance 'report :acceptable acceptable)))
      (is (equal "accepted" (dto:report-acceptable-state
                         (report-to-dto report)))))))

;; ----------------------------------------------------------------------
;; Who may review a report
;; ----------------------------------------------------------------------

(def-fixture reviewing ()
  (with-test-store ()
    (let* ((company (make-instance 'company))
           (other-company (make-instance 'company))
           (user (make-instance 'user))
           (channel (make-instance 'channel :company company))
           (run (make-recorder-run :company company :channel channel))
           (report (make-instance 'report :run run :channel channel))
           (acceptable (make-instance 'base-acceptable :report report)))
      (roles:ensure-has-role company user 'roles:standard-member)
      (flet ((browser (user)
               (make-instance 'normal-viewer-context :user user))
             (api (user company)
               (make-instance 'api-viewer-context
                              :user user
                              :api-key (make-instance 'api-key
                                                      :user user
                                                      :permissions '(:full)
                                                      :company company))))
        (&body)))))

(test a-standard-member-may-review-their-companys-report
  (with-fixture reviewing ()
    (is-true (auth:can-viewer-edit (browser user) acceptable))))

(test a-guest-may-see-a-report-but-may-not-review-it
  (with-fixture reviewing ()
    (roles:ensure-has-role company user 'roles:guest)
    (is-true (auth:can-viewer-view (browser user) acceptable))
    (is-false (auth:can-viewer-edit (browser user) acceptable))))

(test someone-outside-the-company-may-not-review
  (with-fixture reviewing ()
    (let ((outsider (make-instance 'user)))
      (is-false (auth:can-viewer-edit (browser outsider) acceptable)))))

(test an-api-key-may-review-only-its-own-companys-reports
  "A user can belong to several companies; the key is issued for one. Before
CAN-VIEWER-EDIT reached API contexts, the key's company was dropped on the
way through the CAN-EDIT bridge and this second assertion was true."
  (with-fixture reviewing ()
    (roles:ensure-has-role other-company user 'roles:standard-member)
    (is-true (auth:can-viewer-edit (api user company) acceptable))
    (is-false (auth:can-viewer-edit (api user other-company) acceptable))))

(test an-api-key-may-review-a-run-directly-under-the-same-rule
  "The acceptable delegates to its run, so the run is where the rule lives."
  (with-fixture reviewing ()
    (roles:ensure-has-role other-company user 'roles:standard-member)
    (is-true (auth:can-viewer-edit (api user company) run))
    (is-false (auth:can-viewer-edit (api user other-company) run))))

(test reviewing-does-not-go-through-the-legacy-can-edit-bridge
  "CAN-EDIT-WITH-NORMAL-VIEWER-CONTEXT warns by design. Reaching it means
the viewer context was thrown away somewhere, which is how the API key's
company came to be ignored in the first place."
  (with-fixture reviewing ()
    (let ((warnings nil))
      (handler-bind ((warning (lambda (w)
                                (push (princ-to-string w) warnings)
                                (muffle-warning w))))
        (auth:can-viewer-edit (api user company) acceptable)
        (auth:can-viewer-edit (browser user) acceptable))
      (is (equal nil warnings)))))
