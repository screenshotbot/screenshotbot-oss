;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/lparallel
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:export
   #:bind-future
   #:immediate-promise))
(in-package :util/lparallel)

(defvar *sleep-time* 1)

(defun bind-future (promise fn)
  (lparallel:future
    (let ((res (lparallel:force promise)))
      (funcall fn res))))

(defun immediate-promise (val)
  (let ((promise (lparallel:promise)))
    (lparallel:fulfill promise val)
    promise))

(defun wait-for-many (promises)
  "Wait for any of the promises to be fulfilled. If something is already
fulfilled, it will immediately return."
  (cond
   ((eql 1 (length promises))
    (lparallel:force (car promises))
    (car promises))
   (t
    (or
     (loop for promise in promises
           if (lparallel:fulfilledp promise)
             return promise)
     (progn
       (sleep *sleep-time*)
       (wait-for-many promises))))))

(def-easy-macro while-promises (&rest promises &fn fn)
  "While there are unfulfilled PROMISES keep calling FN.

If there are N promises, typically you "
  (labels ((call-next (promises)
             (let ((unfulfilled (loop for promise in promises
                                      unless (lparallel:fulfilledp promise)
                                        collect promise)))
               (fn)
               (cond
                 (unfulfilled
                  ;; Wait for at least one of these to be fulfilled
                  (wait-for-many unfulfilled)
                  (call-next unfulfilled))))))

    (call-next promises)))
