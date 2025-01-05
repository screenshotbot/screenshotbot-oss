;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-abstract-pr-promoter
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:same-pull-request-p
                #:pr-merge-base
                #:make-run-check
                #:push-remote-check-via-batching
                #:check-key
                #:make-check
                #:actual-sha
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
                #:pull-request-url
                #:%created-at
                #:created-at
                #:channel-repo
                #:user
                #:channel)
  (:import-from #:screenshotbot/promote-api
                #:maybe-promote)
  (:import-from #:screenshotbot/model/recorder-run
                #:unchanged-run
                #:make-recorder-run
                #:compared-against
                #:merge-base-failed-warning
                #:recorder-run-warnings
                #:recorder-run)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:is-not
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:screenshotbot/model/report
                #:reports-for-run
                #:report
                #:base-acceptable)
  (:import-from #:screenshotbot/diff-report
                #:diff-report-empty-p)
  (:import-from #:cl-mock
                #:if-called
                #:answer)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:import-from #:screenshotbot/git-repo
                #:commit-graph-dag
                #:commit-graph
                #:compute-merge-base
                #:get-parent-commit)
  (:import-from #:screenshotbot/model/failed-run
                #:failed-run)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:util/logger
                #:format-log)
  (:import-from #:screenshotbot/model/finalized-commit
                #:finalized-commit)
  (:import-from #:screenshotbot/diff-report
                #:diff-report-empty-p)
  (:import-from #:screenshotbot/model/batch
                #:find-or-create-batch)
  (:import-from #:util/threading
                #:make-thread)
  (:import-from #:screenshotbot/screenshot-api
                #:make-screenshot)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:alexandria
                #:remove-from-plist)
  (:import-from #:screenshotbot/model/pr-rollout-rule
                #:pr-rollout-rule-for-company
                #:disable-pull-request-checks-p
                #:pr-rollout-rule)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/dashboard/compare
                #:warmup-report))
(in-package :screenshotbot/test-abstract-pr-promoter)


(util/fiveam:def-suite)

(defvar *lock* (bt:make-lock))
(defvar *cv* (bt:make-condition-variable))

(defconstant +empty-logger+ nil)

(defclass test-logger ()
  ((stream :initform (make-string-output-stream)
           :reader test-logger-stream)))

(defmethod format-log ((logger test-logger)
                       level msg &rest args)
  (apply #'format (test-logger-stream logger)
         msg
         args)
  (format (test-logger-stream logger) "~%"))

(def-fixture state ()
  (with-test-store (:globally t)
    (cl-mock:with-mocks ()
      (with-installation (:globally t)
        (let* ((company (make-instance 'company))
               (channel (make-instance 'channel
                                       :name "foobar-channel"
                                       :company company
                                       :github-repo "https://mysite.git/foo.git"))
               (run (make-recorder-run
                     :company company
                     :channel channel
                     :commit-hash "car"
                     :merge-base "foo"))
               (user (make-instance 'user
                                    :full-name "Arnold"))
               (another-run (make-recorder-run
                             :commit-hash "foo"))
               (promoter (make-instance 'test-promoter)))
          (&body))))))

(test simple-run-retriever-test
  (with-fixture state ()
    (let ((retriever (make-instance 'run-retriever
                                    :sleep-time 0)))
      (is (equal nil (lparallel:force (retrieve-run retriever channel "abcd"
                                                    +empty-logger+)))))))



(test run-retriever-when-commit-has-not-failed-or-finalized
  (with-fixture state ()
    (cl-mock:answer (channel-repo channel) :git-repo)
    (cl-mock:answer (get-parent-commit :git-repo "abcd")
      "foobar")
    (let ((retriever (make-instance 'run-retriever
                                    :sleep-time 0))
          (logger (make-instance 'test-logger)))
      (is (equal nil (lparallel:force
                      (retrieve-run retriever channel "abcd"
                                    logger))))
      (assert-that (get-output-stream-string
                    (test-logger-stream logger))
                   (contains-string "Waiting 0s before checking again")))))

(test run-retriever-when-commit-has-failed
  (with-fixture state ()
    (cl-mock:answer (channel-repo channel) :git-repo)
    (cl-mock:answer (get-parent-commit :git-repo "abcd")
      "foobar")
    (make-instance 'failed-run
                   :channel channel
                   :commit "abcd")
    (let ((retriever (make-instance 'run-retriever
                                    :sleep-time 0))
          (logger (make-instance 'test-logger)))
      (is (equal nil (lparallel:force (retrieve-run retriever channel "abcd"
                                                    logger))))
      (assert-that (get-output-stream-string
                    (test-logger-stream logger))
                   (is-not (contains-string "Waiting 0s before checking again"))))))

(test run-retriever-when-commit-is-finalized
  (with-fixture state ()
    (cl-mock:answer (channel-repo channel) :git-repo)
    (cl-mock:answer (get-parent-commit :git-repo "abcd")
      "foobar")
    (make-instance 'finalized-commit
                   :company company
                   :commit "abcd")
    (let ((retriever (make-instance 'run-retriever
                                    :sleep-time 0))
          (logger (make-instance 'test-logger)))
      (is (equal nil (lparallel:force (retrieve-run retriever channel "abcd"
                                                    logger))))
      (assert-that (get-output-stream-string
                    (test-logger-stream logger))
                   (is-not (contains-string "Waiting 0s before checking again"))))))

(test run-retriever-when-commit-is-finalized-but-the-run-is-present
  (with-fixture state ()
    (cl-mock:answer (channel-repo channel) :git-repo)
    (cl-mock:answer (get-parent-commit :git-repo "abcd")
      "foobar")
    (make-instance 'finalized-commit
                   :company company
                   :commit "abcd")

    (let* ((run (make-recorder-run
                 :commit-hash "abcd"
                 :channel channel))
           (retriever (make-instance 'run-retriever
                                     :sleep-time 0))
           (logger (make-instance 'test-logger)))
      (is (equal run (lparallel:force (retrieve-run retriever channel "abcd"
                                                    logger))))
      (assert-that (get-output-stream-string
                    (test-logger-stream logger))
                   (is-not (contains-string "Waiting 0s before checking again"))))))


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
                         base-commit
                         logger)
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
           (thread (make-thread
                    (lambda ()
                      (maybe-promote promoter run)
                      (setf done t)))))
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
      (answer (warmup-report report))
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

(defclass fake-github-promoter (abstract-pr-promoter)
  ()
  (:documentation "Fake in the sense that it only implements promoter-pull-id"))

(defmethod promoter-pull-id ((self fake-github-promoter) run)
  (pull-request-url run))

(test same-pull-request-p-happy-path
  (with-fixture state ()
   (let ((promoter (make-instance 'fake-github-promoter)))
     (is-true (same-pull-request-p
               promoter
               (make-recorder-run
                :pull-request "foo1")
               (make-recorder-run
                :pull-request "foo1"))))))

(test same-pull-request-p-when-pull-id-is-integer
  "The Phabricator promoter returns integers"
  (with-fixture state ()
    (let ((promoter (make-instance 'fake-github-promoter)))
      (cl-mock:if-called 'promoter-pull-id
                         (lambda (promoter run)
                           22))
      (is-true (same-pull-request-p
                promoter
                (make-recorder-run
                 :pull-request "foo1")
                (make-recorder-run
                 :pull-request "foo1"))))))

(test same-pull-request-p-not-same!
  (with-fixture state ()
   (let ((promoter (make-instance 'fake-github-promoter)))
     (is-false (same-pull-request-p
                promoter
                (make-recorder-run
                 :pull-request "foo2")
                (make-recorder-run
                 :pull-request "foo1"))))))

(test same-pull-request-p-when-pull-request-url-is-not-provided-but-branch-is-same
  (with-fixture state ()
    (let ((promoter (make-instance 'fake-github-promoter)))
      (is-true (same-pull-request-p
                promoter
                (make-recorder-run
                 :author "foo@example.com"
                 :work-branch "carbar"
                 :pull-request "")
                (make-recorder-run
                 :author "foo@example.com"
                 :work-branch "carbar"
                 :pull-request ""))))))


(test if-the-pull-ids-dont-match-then-its-always-false
  (with-fixture state ()
    (let ((promoter (make-instance 'fake-github-promoter)))
      (is-false (same-pull-request-p
                 promoter
                 (make-recorder-run
                  :author "foo@example.com"
                  :work-branch "carbar"
                  :pull-request "foo1")
                 (make-recorder-run
                  :author "foo@example.com"
                  :work-branch "carbar"
                  :pull-request "foo2"))))))

(test if-one-pull-request-url-is-unavable-then-we-still-match-by-branch
  (with-fixture state ()
    (let ((promoter (make-instance 'fake-github-promoter)))
      (is-true (same-pull-request-p
                promoter
                (make-recorder-run
                 :author "foo@example.com"
                 :work-branch "carbar"
                 :pull-request "")
                (make-recorder-run
                 :author "foo@example.com"
                 :work-branch "carbar"
                 :pull-request "foo2"))))))

(test same-pull-request-p-when-pull-request-url-is-not-provided-but-branch-is-not-same
  (with-fixture state ()
    (let ((promoter (make-instance 'fake-github-promoter)))
      (is-false (same-pull-request-p
                 promoter
                 (make-recorder-run
                  :author "foo@example.com"
                  :work-branch "carbar2"
                  :pull-request "")
                 (make-recorder-run
                  :author "foo@example.com"
                  :work-branch "carbar"
                  :pull-request ""))))))

(test same-pull-request-p-ignores-branch-name-when-empty
  (with-fixture state ()
    (let ((promoter (make-instance 'fake-github-promoter)))
      (is-false (same-pull-request-p
                 promoter
                 (make-recorder-run
                  :author "foo@example.com"
                  :work-branch ""
                  :pull-request "")
                 (make-recorder-run
                  :author "foo@example.com"
                  :work-branch ""
                  :pull-request ""))))))

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
    (let ((another-run (make-recorder-run
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

(test make-task-args-override-commit-hash
  (with-fixture state ()
    (let ((run (make-recorder-run
                :channel channel
                :override-commit-hash "foobar"
                :commit-hash "zoidberg")))
      (is (equal "foobar" (actual-sha run))))))

(test make-task-args-no-commit-hash
  (with-fixture state ()
    (let ((run (make-recorder-run
                :channel channel
                :commit-hash "zoidberg")))
      (is (equal "zoidberg" (actual-sha run))))))

(test key-for-make-check
  (with-fixture state ()
    (let ((check (make-check run)))
      (is (equal "foobar-channel" (check-key check))))))

(test if-merge-base-is-null-then-do-nothing
  "If the git merge-base command failed on the CI server, then this will be null."
  (with-fixture state ()
    (let ((run (make-recorder-run
                :company company
                :channel channel
                :merge-base nil
                :commit-hash "foo")))
      (cl-mock:if-called 'retrieve-run
                         (lambda (retriver channel merge-base run)
                           (error "should not be called")))
      (maybe-promote promoter run))))

(test make-run-check
  (with-fixture state ()
    (assert-that (make-run-check run
                                 :status :success
                                 :title "foobar")
                 (has-typep 'check))))

(test on-main-branch-push-a-green-check
  "If the git merge-base command failed on the CI server, then this will be null."
  (with-fixture state ()
    (let ((checks)
          (run (make-recorder-run
                :company company
                :channel channel
                :work-branch "master"
                :branch "master"
                :merge-base "foo"
                :commit-hash "foo")))
      (cl-mock:if-called 'retrieve-run
                         (lambda (retriver channel merge-base run)
                           (error "should not be called")))
      (cl-mock:if-called 'push-remote-check
                         (lambda (promoter run check)
                           (push check checks)))
      (maybe-promote promoter run)
      (assert-that checks
                   (contains (has-typep 'check))))))


(test unchanged-run-without-batch-should-do-nothing
  (with-fixture state ()
    (let ((unchanged-run (make-instance 'unchanged-run
                                        :commit "abdc")))
      (finishes
        (maybe-promote promoter unchanged-run)))))

(def-fixture unchanged-run (&key (work-branch "foobar"))
  (let* ((batch (find-or-create-batch
                 :commit "abcd"
                 :company company))
         (unchanged-run (make-instance 'unchanged-run
                                       :merge-base "car"
                                       :commit "abcd"
                                       :channel channel
                                       :work-branch work-branch
                                       :batch batch)))
    (&body)))

(test unchanged-run-happy-promotion-path
  (with-fixture state ()
    (with-fixture unchanged-run ()
      (let ((promoted nil))
        (if-called 'push-remote-check-via-batching
                   (lambda (&rest args)
                     (setf promoted t)))
        (maybe-promote promoter unchanged-run)
        (is-true promoted)))))

(test unchanged-run-will-still-promote-even-on-master-branch
  "This was an accidental behavior, but is the correct behavior when
supporting merge queues. In particular, since unchanged-runs never
result in reviews, it is safe to promote on non-PR branches. See T1088."
  (with-fixture state ()
    (with-fixture unchanged-run (:work-branch "master")
      (let ((checks))
        (if-called 'push-remote-check
                   (lambda (promoter run check)
                     (push check checks)))
        (maybe-promote promoter unchanged-run)
        (is-true checks)
        (assert-that checks (has-length 1))
        (is (equal "Nothing to review" (check-title (car checks))))))))


(test pr-merge-base-will-compute-from-graph-if-needed
  (with-fixture state ()
    (let ((merge-base-args))
      (if-called 'compute-merge-base
                 (lambda (&rest args)
                   (setf merge-base-args args)
                   "bleh"))
      (let ((run (make-recorder-run
                  :merge-base nil
                  :channel channel
                  :branch-hash "bb"
                  :commit-hash "aa")))
        (is
         (equal "bleh"
                (pr-merge-base
                 promoter
                 run))
         (is (equal (list "aa" "bbc")
                    (cdr merge-base-args))))))))

(test pr-merge-base-integration-happy-path
  (with-fixture state ()
    (let ((merge-base-args))
      (let ((run (make-recorder-run
                  :merge-base nil
                  :channel channel
                  :branch-hash "bb"
                  :commit-hash "aa")))
        (is
         (equal nil
                (pr-merge-base
                 promoter
                 run)))))))

(test previous-review-returns-nil-if-theres-nothing
  (with-fixture state ()
    (let ((run1 (make-recorder-run
                 :channel channel)))
      (answer (promoter-pull-id promoter run1) "foo")
      (is (eql nil (previous-review promoter run1))))))


(test previous-review-returns-the-older-run
  (with-fixture state ()
    (flet ((make-recorder-run (&rest args &key created-at &allow-other-keys)
             (let ((run (apply #'make-recorder-run (remove-from-plist args :created-at))))
               ;; As of this writing, created-at is actually ignored
               ;; because of the base has-created-at class.
               (setf (%created-at run) (+ (get-universal-time)
                                          -2000
                                          created-at))
               run)))
     (let* ((screenshot (make-screenshot
                         :name "foo"
                         :image (make-image :pathname (asdf:system-relative-pathname
                                                       :screenshotbot
                                                       "fixture/rose.png"))))
            (base-run (make-recorder-run
                       :created-at 999
                       :screenshots nil
                       :channel channel))
            (run1 (make-recorder-run
                   :created-at 1000
                   :screenshots (list screenshot)
                   :channel channel))
            (run2 (make-recorder-run
                   :created-at 1003
                   :screenshots (list screenshot)
                   :channel channel))
            (run3 (make-recorder-run
                   :created-at 1005
                   :screenshots (list screenshot)
                   :channel channel)))

       (let* ((acceptable (make-instance 'base-acceptable
                                         :user (make-user)
                                         :state :accepted))
              (report (make-instance 'report
                                     :acceptable acceptable
                                     :run run2
                                     :previous-run base-run)))
         (assert-that (reports-for-run run2)
                      (contains report))
         (cl-mock:if-called 'promoter-pull-id
                            (lambda (promoter run)
                              "foo"))

         (is (eql acceptable (previous-review promoter run3))))))))

(test previous-review-returns-the-older-run-for-branch-too
  (with-fixture state ()
    (flet ((make-recorder-run (&rest args &key created-at &allow-other-keys)
             (let ((run (apply #'make-recorder-run
                               (remove-from-plist args :created-at))))
               ;; As of this writing, created-at is actually ignored
               ;; because of the base has-created-at class.
               (setf (%created-at run) (+ (get-universal-time)
                                          -2000
                                          created-at))
               run)))
     (let* ((screenshot (make-screenshot
                         :name "foo"
                         :image (make-image :pathname (asdf:system-relative-pathname
                                                       :screenshotbot
                                                       "fixture/rose.png"))))
            (base-run (make-recorder-run
                       :created-at 999
                       :screenshots nil
                       :channel channel))
            (run1 (make-recorder-run
                   :created-at 1000
                   :screenshots (list screenshot)
                   :work-branch "foo"
                   :channel channel))
            (run2 (make-recorder-run
                   :created-at 1003
                   :work-branch "foo"
                   :screenshots (list screenshot)
                   :channel channel))
            (run3 (make-recorder-run
                   :created-at 1005
                   :work-branch "foo"
                   :screenshots (list screenshot)
                   :channel channel)))

       (let* ((acceptable (make-instance 'base-acceptable
                                         :user (make-user)
                                         :state :accepted))
              (report (make-instance 'report
                                     :acceptable acceptable
                                     :run run2
                                     :previous-run base-run)))
         (assert-that (reports-for-run run2)
                      (contains report))
         (cl-mock:if-called 'promoter-pull-id
                            (lambda (promoter run)
                              nil))

         (is (eql acceptable (previous-review promoter run3))))))))

(defclass always-blocked-pr-rollout-rule (pr-rollout-rule)
  ()
  (:metaclass persistent-class))

(defmethod disable-pull-request-checks-p ((self always-blocked-pr-rollout-rule)
                                          run)
  t)


(test pr-rollout-rule-blocks-a-request
  (with-fixture state ()
    (let ((checks)
          (run (make-recorder-run
                :company company
                :channel channel
                :work-branch "some-work"
                :branch "master"
                :merge-base "foo"
                :commit-hash "foo")))

      (make-instance 'always-blocked-pr-rollout-rule
                     :company company)

      (is-true (pr-rollout-rule-for-company
                company))

      (cl-mock:if-called 'push-remote-check
                         (lambda (promoter run check)
                           (push check checks)))
      (maybe-promote promoter run)
      (assert-that checks
                   (is-equal-to nil)))))



