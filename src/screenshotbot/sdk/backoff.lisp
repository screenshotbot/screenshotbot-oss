;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/backoff
  (:use #:cl))
(in-package :screenshotbot/sdk/backoff)

(defun backoff (num)
  "A backoff for auto-restart"
  (let ((ret (car (nthcdr num (list 5 10 60)))))
    (warn "Will retry in ~a seconds" ret)
    ret))

(defun add-jitter (num)
  (* num (+ 0.5 (random 1.0))))

(define-condition too-many-requests (error)
  ()
  (:report "We're making too many requests"))

(define-condition server-unavailable (error)
  ()
  (:report "The server is unavailable"))

(defun maybe-retry-request (response-code &key
                                            (attempt (error "must provide :attempt"))
                                            (restart (error "must provide :restart"))
                                            (backoff 2))
  (assert (find-restart restart))
  (when (member response-code '(429 502 503))
    (cond
      ((and
        (< attempt 5)
        auto-restart:*global-enable-auto-retries-p*)
       (let ((timeout (add-jitter (expt backoff (1+ attempt)))))
         (flet ((%warn (message)
                  (log:warn "~a, backing off for ~ds" message (ceiling timeout))))
           (cond
             ((member response-code '(429 503))
              (%warn "We're making too many requests"))
             (t
              (%warn "The server is unavailable"))))
         (sleep timeout)
         (invoke-restart restart)))
      (t
       (cond
         ((member response-code '(429 503))
          (error 'too-many-requests))
         (t
          (error 'server-unavailable)))))))

