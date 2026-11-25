;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :hunchentoot-extensions/test-base-taskmaster
  (:use #:cl
        #:fiveam)
  (:import-from #:hunchentoot-extensions
                #:base-taskmaster
                #:taskmaster-nursery)
  (:import-from #:nursery
                #:with-nursery
                #:start-soon
                #:cancelledp)
  (:import-from #:lparallel
                #:promise
                #:fulfill
                #:force))
(in-package :hunchentoot-extensions/test-base-taskmaster)

(util/fiveam:def-suite)

(defun run-in-nursery (nursery)
  "Start a task in NURSERY, and return what it returns."
  (let ((result (promise)))
    (start-soon nursery (lambda () (fulfill result :done)))
    (force result)))

(defun wait-until-cancelled (nursery)
  (loop repeat 1000
        until (cancelledp nursery)
        do (sleep 0.01))
  (cancelledp nursery))

(test uses-the-nursery-it-was-given
  (with-nursery (nursery)
    (let ((taskmaster (make-instance 'base-taskmaster :nursery nursery)))
      (is (eql nursery (taskmaster-nursery taskmaster))))))

(test falls-back-to-the-background-nursery
  (let ((taskmaster (make-instance 'base-taskmaster)))
    (let ((nursery (taskmaster-nursery taskmaster)))
      (is-true nursery)
      (is (eql nursery (taskmaster-nursery taskmaster)))
      (is (eql nursery (taskmaster-nursery (make-instance 'base-taskmaster))))
      (is (eql :done (run-in-nursery nursery))))))

(test background-nursery-is-replaced-after-a-failure
  (let* ((taskmaster (make-instance 'base-taskmaster))
         (nursery (taskmaster-nursery taskmaster)))
    (let ((nursery:*catch-errors-p* t))
      (start-soon nursery (lambda () (error "failing on purpose"))))
    (is-true (wait-until-cancelled nursery))
    (let ((new-nursery (taskmaster-nursery taskmaster)))
      (is (not (eql nursery new-nursery)))
      (is-false (cancelledp new-nursery))
      (is (eql :done (run-in-nursery new-nursery))))))
