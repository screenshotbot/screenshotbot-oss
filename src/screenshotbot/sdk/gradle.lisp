;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/gradle
  (:use #:cl)
  (:export
   #:main))
(in-package :screenshotbot/sdk/gradle)

(defmacro def-ext-fun (name args &body body)
  `(progn
     (defun ,name ,args
       ,@body
       1)
     (lw:deliver-keep-symbol-names ',name)
     (lw:deliver-keep-symbols ',name)))

(def-ext-fun record-facebook-task (adb)
  (format t "Got adb ~A~%" adb))

(defun read-sym (a)
  (find-symbol
   (str:upcase a)
   #.(find-package :screenshotbot/sdk/gradle)))

(defun main ()
  (uiop:setup-command-line-arguments)
  (let ((args (cdr
               #+lispworks system:*line-arguments-list*
               #-lispworks (uiop:command-line-arguments))))
    (format t "Got args: ~S~%" args)
    (apply (read-sym (car args))
           (cdr args)))
  (uiop:quit 0))
