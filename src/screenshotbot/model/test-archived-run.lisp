;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-archived-run
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/archived-run
                #:archived-run
                #:archived-run-commit
                #:archived-run-branch
                #:archived-run-author
                #:archived-run-cleanp
                #:archived-run-created-at
                #:archived-run-compare-threshold
                #:archived-run-compare-tolerance
                #:archived-run-channel
                #:archived-run-company)
  (:import-from #:json-mop
                #:json-to-clos)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/model/test-archived-run)

(util/fiveam:def-suite)

(test write-and-read-archived-run
  "Test writing an archived-run to JSON and reading it back"
  (let* ((run (make-instance 'archived-run
                             :commit-hash "abc123def456"
                             :branch "main"
                             :author "test-author"
                             :cleanp t
                             :created-at 1234567890
                             :compare-threshold 0.05
                             :compare-tolerance 10
                             :channel "test-channel"
                             :company "test-company"))
         (json-output (with-output-to-string (out)
                        (json-mop:encode run out)))
         (read-run (json-to-clos json-output 'archived-run)))

    ;; Verify the data matches
    (is (equal "abc123def456" (archived-run-commit read-run)))
    (is (equal "main" (archived-run-branch read-run)))
    (is (equal "test-author" (archived-run-author read-run)))
    (is (equal "test-channel" (archived-run-channel read-run)))
    (is (equal "test-company" (archived-run-company read-run)))

    ;; Verify boolean is parsed correctly
    (is (equal t (archived-run-cleanp read-run)))
    (is (typep (archived-run-cleanp read-run) 'boolean))

    ;; Verify integers are parsed correctly
    (is (equal 1234567890 (archived-run-created-at read-run)))
    (is (typep (archived-run-created-at read-run) 'integer))

    ;; Verify numbers are parsed correctly
    (is (< (abs (- 0.05 (archived-run-compare-threshold read-run))) 0.001))
    (is (typep (archived-run-compare-threshold read-run) 'number))
    (is (equal 10 (archived-run-compare-tolerance read-run)))
    (is (typep (archived-run-compare-tolerance read-run) 'number))))

(test write-multiple-archived-runs
  "Test encoding multiple archived-runs to JSON"
  (let* ((run1 (make-instance 'archived-run
                              :commit-hash "commit1"
                              :branch "main"
                              :cleanp t))
         (run2 (make-instance 'archived-run
                              :commit-hash "commit2"
                              :branch "develop"
                              :cleanp nil))
         (json1 (with-output-to-string (out)
                  (json-mop:encode run1 out)))
         (json2 (with-output-to-string (out)
                  (json-mop:encode run2 out)))
         (read-run1 (json-to-clos json1 'archived-run))
         (read-run2 (json-to-clos json2 'archived-run)))

    ;; Verify the data
    (is (equal "commit1" (archived-run-commit read-run1)))
    (is (equal "commit2" (archived-run-commit read-run2)))

    ;; Verify booleans work with both true and false
    (is (equal t (archived-run-cleanp read-run1)))
    (is (typep (archived-run-cleanp read-run1) 'boolean))
    (is (equal nil (archived-run-cleanp read-run2)))
    (is (typep (archived-run-cleanp read-run2) 'boolean))))
