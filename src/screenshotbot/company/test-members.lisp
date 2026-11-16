;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/company/test-members
  (:use #:cl
        #:fiveam)
  (:import-from #:it.bese.fiveam
                #:def-fixture)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:only-instance!
                #:with-test-user)
  (:import-from #:screenshotbot/company/members
                #:user-added-audit-log
                #:new-role
                #:role-changed-audit-log
                #:old-role
                #:can-change-user-role-p
                #:team-page
                #:%delete-invite
                #:%set-user-role)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:screenshotbot/login/recovery-token
                #:make-recovery-token)
  (:import-from #:auth/viewer-context
                #:recovery-viewer-context)
  (:import-from #:screenshotbot/audit-log
                #:activity-log-actor)
  (:import-from #:screenshotbot/model/invite
                #:invite)
  (:import-from #:screenshotbot/user-api
                #:unaccepted-invites)
  (:import-from #:bknr.datastore
                #:object-destroyed-p)
  (:import-from #:screenshotbot/dashboard/audit-log
                #:render-audit-log))
(in-package :screenshotbot/company/test-members)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      (&body))))

(test set-user-role
  (with-fixture state ()
    (finishes
      (%set-user-role company user 'roles:admin))
    (let ((log (only-instance! 'role-changed-audit-log)))
      (is (eql 'roles:standard-member (old-role log)))
      (is (eql 'roles:admin (new-role log))))))

(def-fixture recovery ()
  (with-test-store ()
    (with-installation ()
      (with-fake-request ()
        (auth:with-sessions ()
          (let* ((company (make-instance 'company))
                 (owner (make-user))
                 (admin (make-user))
                 (member (make-user))
                 (recovery-token (make-recovery-token company)))
            (setf (roles:user-role company owner) 'roles:owner)
            (setf (roles:user-role company admin) 'roles:admin)
            (setf (roles:user-role company member) 'roles:standard-member)
            (flet ((use-recovery-viewer-context (&key (company company))
                     (setf (auth:viewer-context hunchentoot:*request*)
                           (make-instance 'recovery-viewer-context
                                          :company company
                                          :recovery-token recovery-token))))
              (&body))))))))

(test recovery-viewer-context-can-change-roles
  (with-fixture recovery ()
    (use-recovery-viewer-context)
    (is-true (can-change-user-role-p company admin))
    (is-true (can-change-user-role-p company member))
    (is-false (can-change-user-role-p company owner))
    (%set-user-role company member 'roles:admin)
    (is-true (roles:has-role-p company member 'roles:admin))
    (is (eql recovery-token
             (activity-log-actor (only-instance! 'role-changed-audit-log))))))

(test recovery-viewer-context-is-only-for-its-company
  (with-fixture recovery ()
    (let ((other (make-instance 'company)))
      (setf (roles:user-role other member) 'roles:standard-member)
      (use-recovery-viewer-context :company other)
      (is-false (can-change-user-role-p company member)))))

(test anonymous-cant-change-roles
  (with-fixture recovery ()
    (is-false (can-change-user-role-p company member))))

(test team-page-with-a-recovery-viewer-context
  (with-fixture recovery ()
    (use-recovery-viewer-context)
    (let ((html (markup:write-html (team-page))))
      (is (str:containsp "Edit role" html))
      (is (not (str:containsp "Pending Invites" html))))))

(test delete-invite-removes-it-from-unaccepted-invites
  (with-fixture state ()
    (let* ((invite (make-instance 'invite
                                  :code "deleted-invite"
                                  :used-p nil
                                  :company company))
           (other-invite (make-instance 'invite
                                        :code "other-invite"
                                        :used-p nil
                                        :company company))
           (other-user (make-user)))
      (setf (unaccepted-invites user) (list invite other-invite))
      (%delete-invite invite)
      (is-true (object-destroyed-p invite))
      (is (equal (list other-invite) (unaccepted-invites user)))
      (is (equal nil (unaccepted-invites other-user))))))

(defun render-to-string (log)
  (markup:write-html (render-audit-log log)))

(test render-user-added-audit-log
  (with-fixture state ()
    (let ((log (make-instance 'user-added-audit-log
                              :user (make-user :email "arnold@example.com")
                              :company company)))
      (is (str:containsp "arnold@example.com"
                         (render-to-string log))))))

(test render-user-added-audit-log-with-unbound-user
  "T2468: a USER-ADDED-AUDIT-LOG whose user slot is unbound shouldn't
break the activity log."
  (with-fixture state ()
    (let ((log (make-instance 'user-added-audit-log
                              :user user
                              :company company)))
      (slot-makunbound log 'screenshotbot/company/members::user)
      (is (str:containsp "(deleted user)"
                         (render-to-string log))))))

(test render-role-changed-audit-log-with-nil-user
  (with-fixture state ()
    (let ((log (make-instance 'role-changed-audit-log
                              :actor user
                              :user nil
                              :company company
                              :old-role 'roles:standard-member
                              :new-role 'roles:admin)))
      (is (str:containsp "(deleted user)"
                         (render-to-string log))))))
