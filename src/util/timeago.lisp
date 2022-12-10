;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/timeago
  (:use #:cl)
  (:import-from #:local-time
                #:format-timestring)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:timeago
   #:human-render-local-time))
(in-package :util/timeago)

(markup:enable-reader)

(defvar *ts-format-cache* (make-hash-table))

(defvar *current-year*
  (local-time:timestamp-year (local-time:now)))

(defun format-ts (timestamp)
  (let ((key (cond
               ((numberp timestamp)
                timestamp)
               (t
                (local-time:timestamp-to-universal timestamp)))))
   (util:or-setf
    (gethash key *ts-format-cache*)
    (format nil
            (format nil "~a"
                    (local-time:universal-to-timestamp key))))))


(defun human-render-local-time (local-time)
  (cond
    ((eql *current-year*
          (local-time:timestamp-year local-time))
     (format-timestring nil local-time
                        :format '(:long-month " " :day)))
    (t
     (format-timestring nil local-time
                        :format '(:long-month " " :day " " :year)))))

(markup:deftag timeago (&key timestamp)
  (multiple-value-bind (key local-time)
      (cond
        ((numberp timestamp)
         (values timestamp (local-time:universal-to-timestamp timestamp)))
        (t
         (values (local-time:timestamp-to-universal timestamp) timestamp)))
    (let* ((timestamp (format-ts key)))
      <:time class= (when (> key (- (get-universal-time) (* 30 24 3600))) "timeago") datetime= timestamp title= (format-timestring nil local-time :format local-time:+rfc-1123-format+) >
      ,(human-render-local-time local-time)
      </:time>)))
