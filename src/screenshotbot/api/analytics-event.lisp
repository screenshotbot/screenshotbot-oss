;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/analytics-event
  (:use #:cl)
  (:import-from #:hunchentoot
                #:raw-post-data)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:util/events
                #:push-event)
  (:import-from #:util/throttler
                #:throttle!
                #:ip-throttler))
(in-package :screenshotbot/api/analytics-event)

(defvar *ip-throttler* (make-instance 'ip-throttler
                                      :tokens 3600))

(defhandler (nil :uri "/api/analytics-event" :method :post) ()
  (parse-all-events (hunchentoot:raw-post-data :force-text t)))

(defun parse-all-events (content)
  (let ((body (yason:parse content)))
    (loop for ev in body do
      (push-event-from-json ev))))

(defun push-event-from-json (ev)
  (throttle! *ip-throttler*)
  (apply
   #'push-event
   :web-event
   :ip-address (hunchentoot:real-remote-addr)
   (loop for key being the hash-keys of ev
         using (hash-value val)
         if (atom val)
           appending (list key val))))
