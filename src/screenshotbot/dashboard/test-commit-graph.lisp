;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-commit-graph
  (:use #:cl
        #:fiveam)
  (:import-from #:dag
                #:add-commit
                #:commit
                #:dag
                #:ordered-commits)
  (:import-from #:screenshotbot/dashboard/commit-graph
                #:format-graph)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:has-length))
(in-package :screenshotbot/dashboard/test-commit-graph)

(util/fiveam:def-suite)

(defun add-edge (dag from to)
  "Add a commit with parents to the DAG."
  (add-commit dag (make-instance 'commit
                                 :sha from
                                 :parents (cond
                                            ((null to) nil)
                                            ((listp to) to)
                                            (t (list to))))))

(defun graph-max-width (graph-str)
  "Calculate the maximum graph width (columns) from output.
   Each column is 2 chars wide (char + space)."
  (let ((max-width 0))
    (loop for line in (str:split #\Newline graph-str)
          do (let* ((graph-part (subseq line 0 (or (position #\Space line
                                                            :start (position-if-not
                                                                    (lambda (c)
                                                                      (member c '(#\* #\| #\/ #\\ #\Space)))
                                                                    line))
                                                   (length line))))
                    ;; Count actual column characters (skip trailing spaces)
                    (trimmed (string-right-trim '(#\Space) graph-part))
                    (width (ceiling (length trimmed) 2)))
               (setf max-width (max max-width width))))
    max-width))

(defun count-graph-columns (graph-str)
  "Count the max number of active columns in the graph output."
  (let ((max-cols 0))
    (loop for line in (str:split #\Newline graph-str)
          when (> (length line) 0)
          do (let ((cols 0))
               ;; Count column markers: * | / \
               (loop for i from 0 below (length line) by 2
                     for c = (char line i)
                     while (member c '(#\* #\| #\/ #\\  #\Space))
                     when (member c '(#\* #\| #\/ #\\))
                     do (incf cols))
               (setf max-cols (max max-cols cols))))
    max-cols))

(test linear-history-uses-one-column
  "A simple linear history should only use 1 column."
  (let ((dag (make-instance 'dag)))
    (add-edge dag "aa" nil)
    (add-edge dag "bb" "aa")
    (add-edge dag "cc" "bb")
    (add-edge dag "dd" "cc")
    (let* ((commits (ordered-commits dag))
           (output (format-graph commits)))
      (is (= 1 (count-graph-columns output))
          "Linear history should use exactly 1 column, got output:~%~a" output))))

(test simple-branch-merge-uses-two-columns
  "A branch that merges back should use at most 2 columns."
  (let ((dag (make-instance 'dag)))
    ;; Create:  dd
    ;;         /  \
    ;;        bb  cc
    ;;         \  /
    ;;          aa
    (add-edge dag "aa" nil)
    (add-edge dag "bb" "aa")
    (add-edge dag "cc" "aa")
    (add-edge dag "dd" (list "bb" "cc"))
    (let* ((commits (ordered-commits dag))
           (output (format-graph commits)))
      (is (<= (count-graph-columns output) 2)
          "Branch+merge should use at most 2 columns, got output:~%~a" output))))

(test lanes-are-reused-after-merge
  "After a branch merges, its lane should be reused for new branches."
  (let ((dag (make-instance 'dag)))
    ;; Create multiple sequential merges that should reuse columns:
    ;;   ff
    ;;   |
    ;;   ee (merge bb+cc)
    ;;  /|
    ;; bb cc
    ;;  \|
    ;;   dd (merge)
    ;;   |
    ;;   aa
    (add-edge dag "aa" nil)
    (add-edge dag "bb" "aa")
    (add-edge dag "cc" "aa")
    (add-edge dag "dd" (list "bb" "cc"))  ; First merge
    (add-edge dag "ee" "dd")
    (add-edge dag "ff" "ee")
    (let* ((commits (ordered-commits dag))
           (output (format-graph commits)))
      ;; After the merge at dd, we should be back to 1 column
      ;; The later commits ee, ff should not expand to more columns
      (is (<= (count-graph-columns output) 2)
          "Lanes should be reused after merge, got output:~%~a" output))))

(test multiple-merges-reuse-lanes
  "Multiple sequential merges should reuse lanes, not keep adding new ones."
  (let ((dag (make-instance 'dag)))
    ;; aa (root)
    ;; bb <- aa
    ;; cc <- aa  (branch)
    ;; dd <- bb, cc (merge 1)
    ;; ee <- dd
    ;; ff <- dd  (branch)
    ;; gg <- ee, ff (merge 2)
    (add-edge dag "aa" nil)
    (add-edge dag "bb" "aa")
    (add-edge dag "cc" "aa")
    (add-edge dag "dd" (list "bb" "cc"))
    (add-edge dag "ee" "dd")
    (add-edge dag "ff" "dd")
    (add-edge dag "gg" (list "ee" "ff"))
    (let* ((commits (ordered-commits dag))
           (output (format-graph commits)))
      ;; With proper lane reuse, should never need more than 2 columns
      (is (<= (count-graph-columns output) 2)
          "Multiple merges should reuse lanes, got output:~%~a" output))))

(test print-graph-output
  "Helper test to visualize graph output."
  (let ((dag (make-instance 'dag)))
    (add-edge dag "aa" nil)
    (add-edge dag "bb" "aa")
    (add-edge dag "cc" "aa")
    (add-edge dag "dd" (list "bb" "cc"))
    (add-edge dag "ee" "dd")
    (add-edge dag "ff" "dd")
    (add-edge dag "gg" (list "ee" "ff"))
    (let* ((commits (ordered-commits dag))
           (output (format-graph commits)))
      (format t "~%Graph output:~%~a~%" output)
      (format t "Max columns: ~a~%" (count-graph-columns output))
      (pass))))
