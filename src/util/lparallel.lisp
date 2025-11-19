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
   #:immediate-promise
   #:with-temp-lparallel-kernel))
(in-package :util/lparallel)

(defvar *sleep-time* 1)

(defun immediate-promise (val)
  (let ((promise (lparallel:promise)))
    (lparallel:fulfill promise val)
    promise))

(def-easy-macro with-temp-lparallel-kernel (&key (threads 20)
                                                 &fn fn)
  (let ((shutting-down-p nil))
    (let ((lparallel:*kernel* (lparallel:make-kernel threads
                                                     #+nil #+nil
                                                     :context #'context)))
      
      (unwind-protect
           (funcall fn)
        (lparallel:end-kernel :wait t)))))
