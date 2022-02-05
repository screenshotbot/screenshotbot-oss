;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/magick
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:run-magick))
(in-package :screenshotbot/magick)

(defvar *semaphore* (bt:make-semaphore
                     :name "magick"
                     :count 20))

(defun call-with-semaphore (sem fn)
  (unwind-protect
       (progn
         (bt:wait-on-semaphore sem :timeout 60)
         (funcall fn))
    (bt:signal-semaphore sem)))

(defun magick-prefix-uncached ()
  (multiple-value-bind (out err ret)
      (uiop:run-program (list "magick" "--help")
                        :ignore-error-status t)
    (declare (ignore out err))
    (cond
      ((= 0 ret)
       (list "magick")))))

(let (cache)
 (defun magick-prefix ()
   "ImageMagick 7 comes with a binary called `magick`. We can still
  call `convert` etc directly. But `convert` *might* point to a 6.9
  version of ImageMagick. The 6.9 version is buggy.

  We could download the magick binary at runtime, however, the magick
  binary uses AppImage, and doesn't work seamlessly on docker. In
  production, we'll ensure the magick binary is available. On OSS,
  it's harder to enforce, so for now, if it's not available then we'll
  just revert to calling convert directly."

   (cdr ;; remove :dummy
    (util:or-setf
     cache
     (cons :dummy
           (magick-prefix-uncached))))))

(defun run-magick (command &rest args &key (error-output t) (output t)
                             (ignore-error-status nil))
  "Wrapper for magick commands, in the future we might run this in-process"
  (restart-case
      (call-with-semaphore
       *semaphore*
       (lambda ()
        (let ((command (loop for str in command
                             if (pathnamep str)
                               collect (namestring str)
                             else
                               collect str)))

          (uiop:run-program
           (append (magick-prefix) command)
           :output output
           :error-output error-output
           :ignore-error-status ignore-error-status))))
    (retry-run-magick ()
      (apply #'run-magick command args))))
