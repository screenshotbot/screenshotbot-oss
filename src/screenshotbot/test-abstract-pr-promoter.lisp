;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-abstract-pr-promoter
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:warn-if-not-merge-base
                #:format-check-title
                #:make-check-for-report
                #:check-title
                #:previous-review
                #:make-acceptable
                #:promoter-pull-id
                #:check-status
                #:make-check-result-from-diff-report
                #:check
                #:plugin-installed?
                #:valid-repo?
                #:push-remote-check
                #:abstract-pr-promoter
                #:format-updated-summary
                #:retrieve-run
                #:run-retriever)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/user-api
                #:channel-repo
                #:user
                #:channel)
  (:import-from #:util/testing
                #:with-global-binding)
  (:import-from #:screenshotbot/promote-api
                #:maybe-promote)
  (:import-from #:screenshotbot/model/recorder-run
                #:compared-against
                #:merge-base-failed-warning
                #:recorder-run-warnings
                #:recorder-run)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:screenshotbot/model/report
                #:base-acceptable)
  (:import-from #:screenshotbot/dashboard/compare
                #:diff-report-empty-p)
  (:import-from #:cl-mock
                #:answer)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:import-from #:screenshotbot/git-repo
                #:get-parent-commit)
  (:import-from #:screenshotbot/model/failed-run
                #:failed-run)
  (:import-from #:fiveam-matchers/has-length
                #:has-length))
(in-package :screenshotbot/test-abstract-pr-promoter)


(util/fiveam:def-suite)

(defvar *lock* (bt:make-lock))
(defvar *cv* (bt:make-condition-variable))

(def-fixture state ()
  (with-test-store ()
    (with-global-binding ((lparallel:*kernel* (lparallel:make-kernel 2)))
      (cl-mock:with-mocks ()
       (with-installation (:globally t)
         (let* ((company (make-instance 'company))
                (channel (make-instance 'channel
                                        :company company
                                        :github-repo "https://mysite.git/foo.git"))
                (run (make-instance 'recorder-run
                                    :company company
                                    :channel channel
                                    :commit-hash "car"
                                    :merge-base "foo"))
                (user (make-instance 'user
                                     :full-name "Arnold"))
                (another-run (make-instance 'recorder-run
                                            :commit-hash "foo"))
                (promoter (make-instance 'test-promoter)))
           (&body)))))))

(test simple-run-retriever-test
  (with-fixture state ()
    (let ((retriever (make-instance 'run-retriever
                                    :sleep-fn #'identity)))
      (is (equal nil (lparallel:force (retrieve-run retriever channel "abcd")))))))

(test run-retriever-when-commit-has-failed
  (with-fixture state ()
    (cl-mock:answer (channel-repo channel) :git-repo)
    (cl-mock:answer (get-parent-commit :git-repo "abcd")
      "foobar")
    (make-instance 'failed-run
                   :channel channel
                   :commit "abcd")
    (let ((retriever (make-instance 'run-retriever
                                    :sleep-fn #'identity)))
      (is (equal nil (lparallel:force (retrieve-run retriever channel "abcd")))))))


(test format-updated-summary
  (with-fixture state ()
    (let ((user (make-instance 'user
                               :full-name "Arnold Noronha")))
      (is (equal "accepted by Arnold Noronha"
                 (format-updated-summary
                  :accepted user))))
    (let ((user (make-instance 'user
                               :email "arnold@screenshotbot.io")))
      (is (equal "rejected by arnold@screenshotbot.io"
                 (format-updated-summary
                  :rejected user))))))

(defclass controlled-run-retreiver ()
  ((promise :initarg :promise
            :reader %promise)))

(defclass test-promoter (abstract-pr-promoter)
  ((checks :accessor checks
           :initform nil)))

(defmethod push-remote-check ((promoter test-promoter)
                              run check)
  (bt:with-lock-held (*lock*)
    (push check (checks promoter))
    (bt:condition-notify *cv*)))

(defmethod retrieve-run ((self controlled-run-retreiver)
                         channel
                         base-commit)
  (%promise self))

(defmethod valid-repo? ((self test-promoter) repo)
  t)

(defmethod plugin-installed? ((self test-promoter) company repo-url)
  t)

(test updates-pending-state
  (with-fixture state ()
    (let* ((promise (lparallel:promise))
           (promoter (make-instance 'test-promoter
                                    :run-retriever (make-instance 'controlled-run-retreiver
                                                                  :promise promise)))
           (done nil)
           (thread (bt:make-thread
                    (lambda ()
                      (maybe-promote promoter run)
                      (setf done t))
                    :name "pending-state")))
      (loop until (checks promoter)
            for i from 0 to 100
            do (sleep 0.1))

      (assert-that (checks promoter)
                   (contains (has-typep 'check)))

      (lparallel:fulfill
          promise
        another-run)
      (bt:join-thread thread)

      (assert-that (checks promoter)
                   (contains (has-typep 'check)
                             (has-typep 'check)))
      (is-true done))))

(test previously-accepted-pr
  (with-fixture state ()
    (let* ((old-user (make-instance 'user))
           (old-acc (make-instance 'base-acceptable
                                   :state :rejected
                                   :user old-user))
           (promoter (make-instance 'test-promoter)))
      (answer (promoter-pull-id promoter run)
        20)
      (answer (make-acceptable promoter report)
        (make-instance 'base-acceptable))
      (cl-mock:if-called 'diff-report-empty-p
        (lambda (diff-report)
          nil))
      (answer (previous-review promoter run)
        (make-instance 'base-acceptable
                       :state :rejected
                       :user user))
      (let ((check
              (make-check-result-from-diff-report
               promoter
               run
               another-run)))
        (is (eql :rejected (check-status check)))
        (assert-that (check-title check)
                     (contains-string "previously rejected by Arnold"))))))

(test make-check-for-report
  (with-fixture state ()
    (is (equal "1 changes, accepted by Arnold"
               (format-check-title "1 changes"
                                   :status :accepted
                                   :user user)))
    (is (equal "1 changes, previously rejected by Arnold"
               (format-check-title "1 changes"
                                   :status :rejected
                                   :previousp t
                                   :user user)))
    (is (equal "1 changes"
               (format-check-title "1 changes"
                                   :status :success)))))

(test warn-if-not-merge-base
  (with-fixture state ()
    (let ((another-run (make-instance 'recorder-run
                                      :commit-hash "bleh")))
      (warn-if-not-merge-base
       promoter
       run
       another-run)
      (assert-that (recorder-run-warnings run)
                   (contains
                    (has-typep 'merge-base-failed-warning)))
      (assert-that (mapcar #'compared-against (recorder-run-warnings run))
                   (contains
                    another-run)))))

(test warn-if-not-merge-base-does-nothing-in-the-good-path
  (with-fixture state ()
    (warn-if-not-merge-base
     promoter
     run
     another-run)
    (assert-that (recorder-run-warnings run)
                 (has-length 0))))
