;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/insights/test-pull-requests
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/insights/pull-requests
                #:canonical-pr-url
                #:pr-to-actions
                #:pr-to-run-states
                #:safe-pr
                #:write-pr-actions-csv)
  (:import-from #:screenshotbot/model/channel
                #:channel)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/model/report
                #:base-acceptable
                #:report
                #:report-acceptable)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:util/store/store
                #:with-test-store))
(in-package :screenshotbot/insights/test-pull-requests)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test canonical-pr-url-is-nil-when-we-cant-canonicalize
  "A channel without a repo can't tell us the forge, so
GET-CANONICAL-PULL-REQUEST-URL falls back to its default method. We
must not use that default, since every such run would collapse into a
single \"#\" key."
  (with-fixture state ()
    (let ((run (make-recorder-run
                :channel (make-instance 'channel :name "channel-0")
                :pull-request "git@github.com:foo/bar.git/pull/42")))
      (is (eql nil (canonical-pr-url run)))
      (is (equal "git@github.com:foo/bar.git/pull/42"
                 (safe-pr run))))))

(test safe-pr-is-nil-without-a-pull-request
  (with-fixture state ()
    (let ((run (make-recorder-run
                :channel (make-instance 'channel :name "channel-0"))))
      (is (eql nil (safe-pr run))))))

(defun make-run-with-state (company channel pr state)
  "Make a run on PR with one report whose acceptable is in STATE. STATE
can be NIL for a report that was never reviewed, or :NONE for a run
that had no changes at all (and hence no report)."
  (let ((run (make-recorder-run
              :company company
              :channel channel
              :pull-request pr)))
    (unless (eql :none state)
      (let ((report (make-instance 'report
                                   :run run
                                   :channel channel)))
        (let ((acceptable (make-instance 'base-acceptable
                                         :report report
                                         :state state)))
          (bknr.datastore:with-transaction ()
            (setf (report-acceptable report) acceptable)))))
    run))

(test all-the-run-states-of-a-pr-are-listed-in-order
  (with-fixture state ()
    (let* ((company (make-instance 'company))
           (channel (make-instance 'channel :name "channel-0" :company company))
           (pr "https://github.com/tdrhq/fast-example/pull/20"))
      (dolist (state (list :none :rejected :none :accepted))
        (make-run-with-state company channel pr state))
      ;; Another PR, to make sure we don't mix up the two.
      (make-run-with-state company channel
                           "https://github.com/tdrhq/fast-example/pull/21"
                           nil)
      (let ((states (pr-to-run-states company :num-days 30)))
        (is (equal (list :none :rejected :none :accepted)
                   (gethash pr states)))
        (is (equal (list :changed)
                   (gethash "https://github.com/tdrhq/fast-example/pull/21"
                            states)))))))

(test runs-without-a-pull-request-dont-show-up-as-actions
  "A run with no PR has nothing to attribute its reports to. If it leaks
into PR-TO-ACTIONS it shows up as a NIL key, and
GENERATE-PULL-REQUESTS-CHART's ECASE falls through on it."
  (with-fixture state ()
    (let* ((company (make-instance 'company))
           (channel (make-instance 'channel :name "channel-0" :company company)))
      (dolist (state (list nil :rejected :accepted))
        (make-run-with-state company channel nil state))
      (let ((actions (pr-to-actions company :num-days 30)))
        (is (eql 0 (hash-table-count actions)))
        (is (equal nil (loop for state being the hash-values of actions
                             unless (member state '(:accepted :rejected :changed :none))
                               collect state)))))))

(test run-states-column-in-the-csv
  (with-installation ()
   (with-fixture state ()
     (let* ((company (make-instance 'company))
            (channel (make-instance 'channel :name "channel-0" :company company)))
       (dolist (state (list :none :rejected :none :accepted))
         (make-run-with-state company channel
                              "https://github.com/tdrhq/fast-example/pull/20"
                              state))
       (let ((csv (with-output-to-string (out)
                    (write-pr-actions-csv company out :num-days 30))))
         (is (str:containsp "RUN STATES" csv))
         (is (str:containsp ",No changes; Rejected; No changes; Accepted"
                            csv)))))))
