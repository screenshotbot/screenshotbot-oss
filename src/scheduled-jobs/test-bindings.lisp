;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :scheduled-jobs/test-bindings
  (:use #:cl
        #:fiveam
        #:scheduled-jobs/bindings)
  (:import-from #:scheduled-jobs/bindings
                #:fli-cron-expr)
  (:local-nicknames (#:a #:alexandria)))
(in-package :scheduled-jobs/test-bindings)


(util/fiveam:def-suite)

(test preconditions
  (is-true (cron-parse-expr "* * * * * *"))
  (is-true (fli-cron-expr (cron-parse-expr "* * * * * *"))))

(test parse-invalid
  (signals invalid-cron-expr
    (cron-parse-expr "dfdfsdf")))

;;;; NOTE!!!!  decode|encode-universal-time use the negative of the
;;;; standard time-zone offset! For instance, New York is technically,
;;;; GMT-5:00, but in CL, that would be considered timezone of 5.

(test cron-next
  (handler-bind ((error (lambda (e)
                          (trivial-backtrace:print-backtrace e))))
   (let ((cron-expr (cron-parse-expr "0 0 8 * * *"))
         (time-zone -4))
     (is (equal (multiple-value-list
                 (decode-universal-time
                  (encode-universal-time
                   0 0 8 2 1 2021 (- time-zone))))
                (multiple-value-list
                 (decode-universal-time
                  (cron-next cron-expr
                             :now (encode-universal-time 0 0 6 2 1 2021
                                                         (- time-zone))
                             :timezone time-zone))))))))

(test cron-next-if-in-current-timezone-we-would-hit-sooner-than-gmt
  (handler-bind ((error (lambda (e)
                          (trivial-backtrace:print-backtrace e))))
   (let ((cron-expr (cron-parse-expr "0 0 8 * * *"))
         (time-zone 6))
     (is (equal (multiple-value-list
                 (decode-universal-time
                  (encode-universal-time
                   0 0 8 2 1 2021 (- time-zone))))
                (multiple-value-list
                 (decode-universal-time
                  (cron-next cron-expr
                             :now (encode-universal-time 0 0 6 2 1 2021
                                                         (- time-zone))
                             :timezone time-zone))))))))
