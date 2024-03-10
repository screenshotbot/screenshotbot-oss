;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/review-policy
  (:use #:cl)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-author
                #:recorder-run)
  (:import-from #:screenshotbot/report-api
                #:report-run
                #:report)
  (:export
   #:can-review?
   #:anyone-can-review))
(in-package :screenshotbot/model/review-policy)

(defclass anyone-can-review ()
  ())

(defmethod can-review? ((self anyone-can-review) (run recorder-run) user)
  t)

(defmethod can-review? (policy (report report) user)
  (can-review? policy (report-run report) user))

(defclass disallow-author-review-policy ()
  ())

(defun parse-email (email)
  (when email
   (multiple-value-bind (res parts)
       (cl-ppcre:scan-to-strings ".* <(.*)>" (str:trim email))
     (cond
       (res
        (elt parts 0))
       (t
        email)))))

(defmethod can-review? ((self disallow-author-review-policy) (run recorder-run) user)
  (let ((author (parse-email (recorder-run-author run)))
        (email (auth:user-email user)))
    (cond
      (t
       (not (string-equal (str:trim email) (str:trim author)))))))
