;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/cron
  (:use #:cl)
  (:import-from #:alexandria
                #:remove-from-plist)
  (:import-from #:util/threading
                #:with-tags
                #:ignore-and-log-errors)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:def-cron
   #:cron-enabled-on-store-p))
(in-package :util/cron)

#+nil
(defun funcall-with-wrapper (body)
  (restart-case
      (handler-bind ((error (lambda (e)
                              (cond
                                (*debugger-hook*
                                 (invoke-debugger e))
                                (t
                                 (invoke-restart 'cl:abort))))))
        (funcall body))
    (cl:abort ()
      (format t "abort called~%")
      (values))))

(defun call-with-cron-wrapper (fn)
  (ignore-and-log-errors ()
    (with-tags (("hostname" (uiop:hostname)))
      (util/threading:call-with-thread-fixes fn))))

(defmethod cron-enabled-on-store-p (store)
  t)

(defmacro def-cron (name (&rest args &key (only-on-leader t) &allow-other-keys) &body body)
  "If ONLY-ON-LEADER is true then we only run this cron job on the leader."
  (let ((args (remove-from-plist args :only-on-leader)))
   `(cl-cron:make-cron-job
     (lambda ()
       (when (and
              (boundp 'bknr.datastore:*store*)
              (or
               (not ,only-on-leader)
               (cron-enabled-on-store-p bknr.datastore:*store*)))
         (flet ((body () ,@body))
           (call-with-cron-wrapper #'body))))
     :hash-key ',name
     ,@args)))

#+lispworks
(def-cron gc (:minute 14 :hour 6)
  (hcl:gc-generation t))
