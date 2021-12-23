;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;;;;

(defpackage :screenshotbot/injector
  (:use #:cl)
  (:import-from #:clues/injector
                #:get-instance)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:with-injection
   #:screenshotbot-injector))
(in-package :screenshotbot/injector)

;;;; As of writing, the CLUES injection library is experimental, and
;;;; something I'm working on. Not every component of Screenshotbot
;;;; uses injection, but I'm working on moving it to injection for
;;;; testability reasons. This is the top level injector.

(defclass screenshotbot-injector (clues:injector)
  ())

(defvar *injector* (make-instance 'screenshotbot-injector))

(defmacro with-injection ((&rest args) &body body)
  (let ((args (loop for x in args
                    if (listp x)
                      collect x
                    else
                      collect (list x x))))
    `(let ,(loop for (var class) in args
                 collect `(,var (get-instance *injector* ',class)))
       ,@body)))
