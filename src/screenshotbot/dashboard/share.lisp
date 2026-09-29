;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/share
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro))
(in-package :screenshotbot/dashboard/share)

(def-easy-macro with-expiration-validation (expiry-date &key &binding errors &fn fn)
  (let ((errors))
    (flet ((check (field check message)
             (unless check
               (push (cons field message) errors))))
      (unless (str:emptyp expiry-date)
        (let ((parsed (local-time:parse-timestring expiry-date)))
          (check :expiry-date parsed "Invalid date")
          (when parsed
            (or
             (check :expiry-date
                    (local-time:timestamp>
                     parsed
                     (local-time:now))
                    "Date can't be in the past")
             (check :expiry-date
                    (local-time:timestamp>
                     parsed
                     (local-time:timestamp+ (local-time:now) 2 :day))
                    "Choose a date at least two days in the future"))))
        (check :expiry-date
               (cl-ppcre:scan "\\d{4}-\\d{2}-\\d{2}"
                expiry-date)
               "Invalid date format, perhaps you're using an old browser? Try YYYY-MM-DD format."))

      (fn errors))))


