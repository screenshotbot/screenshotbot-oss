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
                #:archived-run-compare-tolerance)
  (:import-from #:bknr.impex
                #:parse-xml-file
                #:write-to-xml)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/model/test-archived-run)

(util/fiveam:def-suite)

(test write-and-read-archived-run
  "Test writing an archived-run to XML and reading it back"
  (uiop:with-temporary-file (:stream s :pathname p :direction :output
                                      :keep t :type "xml")
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
           (xml-output (with-output-to-string (out)
                         (write-to-xml (list run) :name "archived-runs" :output out))))

      ;; Write XML to file
      (write-string xml-output s)
      (finish-output s)
      (close s)

      ;; Read it back
      (let* ((parsed (parse-xml-file p (list (find-class 'archived-run))))
             (runs (getf parsed :archived-run)))

        ;; Verify we got one run back
        (is (= 1 (length runs)))

        (let ((read-run (first runs)))
          ;; Verify the data matches
          (is (equal "abc123def456" (archived-run-commit read-run)))
          (is (equal "main" (archived-run-branch read-run)))
          (is (equal "test-author" (archived-run-author read-run)))

          ;; Verify boolean is parsed correctly
          (is (equal t (archived-run-cleanp read-run)))
          (is (typep (archived-run-cleanp read-run) 'boolean))

          ;; Verify integers are parsed correctly
          (is (equal 1234567890 (archived-run-created-at read-run)))
          (is (typep (archived-run-created-at read-run) 'integer))

          ;; Verify numbers are parsed correctly
          (is (equal 0.05 (archived-run-compare-threshold read-run)))
          (is (typep (archived-run-compare-threshold read-run) 'number))
          (is (equal 10 (archived-run-compare-tolerance read-run)))
          (is (typep (archived-run-compare-tolerance read-run) 'number)))))))

(test write-multiple-archived-runs
  "Test writing multiple archived-runs to XML and reading them back"
  (uiop:with-temporary-file (:stream s :pathname p :direction :output
                                      :keep t :type "xml")
    (let* ((run1 (make-instance 'archived-run
                                :commit-hash "commit1"
                                :branch "main"
                                :cleanp t))
           (run2 (make-instance 'archived-run
                                :commit-hash "commit2"
                                :branch "develop"
                                :cleanp nil))
           (xml-output (with-output-to-string (out)
                         (write-to-xml (list run1 run2) :name "archived-runs" :output out))))

      ;; Write XML to file
      (write-string xml-output s)
      (finish-output s)
      (close s)

      ;; Read it back
      (let* ((parsed (parse-xml-file p (list (find-class 'archived-run))))
             (runs (getf parsed :archived-run)))

        ;; Verify we got two runs back
        (is (= 2 (length runs)))

        ;; Verify the data
        (is (equal "commit1" (archived-run-commit (first runs))))
        (is (equal "commit2" (archived-run-commit (second runs))))

        ;; Verify booleans work with both true and false
        (is (equal t (archived-run-cleanp (first runs))))
        (is (typep (archived-run-cleanp (first runs)) 'boolean))
        (is (equal nil (archived-run-cleanp (second runs))))
        (is (typep (archived-run-cleanp (second runs)) 'boolean))))))


