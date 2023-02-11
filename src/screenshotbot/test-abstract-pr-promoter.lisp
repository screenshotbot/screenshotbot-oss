;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-abstract-pr-promoter
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/abstract-pr-promoter
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
                #:user
                #:channel)
  (:import-from #:util/testing
                #:with-global-binding)
  (:import-from #:screenshotbot/promote-api
                #:maybe-promote)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains))
(in-package :screenshotbot/test-abstract-pr-promoter)


(util/fiveam:def-suite)

(defvar *lock* (bt:make-lock))
(defvar *cv* (bt:make-condition-variable))

(def-fixture state ()
  (with-test-store ()
    (with-global-binding ((lparallel:*kernel* (lparallel:make-kernel 2)))
      (with-installation (:globally t)
        (let* ((company (make-instance 'company))
               (channel (make-instance 'channel
                                       :company company
                                       :github-repo "https://mysite.git/foo.git"))
               (run (make-instance 'recorder-run
                                   :company company
                                   :channel channel
                                   :commit-hash "car"))
               (another-run (make-instance 'recorder-run)))
         (&body))))))

(test simple-run-retriever-test
  (with-fixture state ()
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
