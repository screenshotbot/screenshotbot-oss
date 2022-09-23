;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/auto-cleanup
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:util/cron
                #:def-cron)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:register-auto-cleanup))
(in-package :screenshotbot/model/auto-cleanup)

(defvar *cleanups* nil)

(defclass cleanup ()
  ((type :initarg :type
         :reader cleanup-type)
   (timestamp :initarg :timestamp
              :reader cleanup-timestamp)
   (age :initarg :age
        :reader cleanup-age)))

(defun register-auto-cleanup (obj &key (timestamp (error "must provide timestamp function"))
                                    (age 30))
  (setf (a:assoc-value *cleanups* obj)
        (make-instance 'cleanup
                       :type obj
                       :timestamp timestamp
                       :age age)))

(defun process-cleanup (cleanup)
  (let ((threshold (- (get-universal-time) (* 24 3600 (cleanup-age cleanup))))
        (objs (class-instances (cleanup-type cleanup))))
    (dolist (obj objs)
      (when (< (funcall (cleanup-timestamp cleanup) obj)
               threshold)
        (delete-object obj)))))

(defun dispatch-cleanups ()
  (loop for (nil . cleanup) in *cleanups* do
    (process-cleanup cleanup)))

(def-cron dispatch-cleanups ()
  (dispatch-cleanups))
