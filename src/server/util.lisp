;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/util
  (:use #:cl))
(in-package :server/util)

#+(and lispworks linux)
(defun safe-load-system (name &key fix-by-default)
  (let ((counter 0))
   (bt:with-lock-held ((symbol-value (read-from-string "bknr.cluster/server::*commit-lock*")))
     (handler-bind ((error
                      (lambda (e)
                        (when (and (typep e 'simple-error)
                                   (str:containsp "is not congruent with" (format nil "~a" e))
                                   ;; Could we loop forever?
                                   (< counter 5))
                          (incf counter)
                          ;; Usually this means "modify the lambda list and delete existing methods"
                          (invoke-restart 'continue)))))
       (asdf:load-system name)))))
