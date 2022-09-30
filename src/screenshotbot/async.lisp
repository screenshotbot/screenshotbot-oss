;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/async
  (:use #:cl)
  (:import-from #:util/macros
                #:def-easy-macro)
  (:import-from #:server
                #:*shutdown-hooks*)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:define-channel
   #:with-screenshotbot-kernel))
(in-package :screenshotbot/async)

(defvar *kernel* nil)

(defvar *kernel-lock* (bt:make-lock))

(defvar *channel-lock* (bt:make-lock))

(defun async-kernel ()
  (util:or-setf
   *kernel*
   (lparallel:make-kernel 8 :name "default-screenshotbot-kernel")
   :thread-safe t
   :lock *kernel-lock*))

(defun make-channel (&rest args)
  (let ((lparallel:*kernel* (async-kernel)))
    (lparallel:make-channel )))

(def-easy-macro with-screenshotbot-kernel (&fn fn)
  (let ((lparallel:*kernel* (async-kernel)))
    (funcall fn)))

(defmacro define-channel (name &rest args)
  (let ((var (intern (format nil "*~a-VAR*" (string name))))
        (fun (intern (format nil "~a-CREATOR" (string name)))))
    `(progn
       (defvar ,var nil)
       (defun ,fun ()
         (util:or-setf
          ,var
          (with-screenshotbot-kernel ()
           (lparallel:make-channel ,@args))
          :thread-safe t
          :lock *channel-lock*))
       (define-symbol-macro ,name
           (,fun)))))

(defun shutdown ()
  (with-screenshotbot-kernel ()
    (log:info "Shutting down: screenshotbot lparallel kernel")
    (lparallel:end-kernel :wait t)
    (setf *kernel* nil)
    (log:info "Done: screenshotbot lparallel kernel")))

(pushnew 'shutdown *shutdown-hooks*)
