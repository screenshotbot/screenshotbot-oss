;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/delayed-accessors
  (:nicknames #:util/delayed-accessors)
  (:use #:cl)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:bknr.datastore
                #:with-transaction))
(in-package :util/delayed-accessors)

(defvar *lock* (bt:make-lock "delayed-acc"))

(defmacro def-delayed-accessor (name accessor &key (step-min 1))
  (let ((cache-var (intern (format nil "*~a-CACHE*" (string name))))
        (cron-name (intern (format nil "FLUSH-~a" (string name)))))
   `(progn
      (defvar ,cache-var nil)
      (defmethod ,name (obj)
        (bt:with-lock-held (*lock*)
         (let ((assoc (assoc obj ,cache-var)))
           (cond
             (assoc
              (cdr assoc))
             (t
              (,accessor obj))))))

      (defmethod (setf ,name) (val obj)
        (bt:with-lock-held (*lock*)
         (push
          (cons obj val)
          ,cache-var)))

      (defun ,cron-name ()
        (let ((cache
                (bt:with-lock-held (*lock*)
                  (prog1
                      (remove-duplicates
                       ,cache-var
                       :key #'car
                       :from-end t)
                    (setf ,cache-var nil)))))
          (with-transaction ()
            (loop for (obj . value) in cache
                  do (setf (,accessor obj)
                           value)))))

      (def-cron ,cron-name (:step-min ,step-min)
        (,cron-name)))))
