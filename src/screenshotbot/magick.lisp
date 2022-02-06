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
      (uiop:run-program (list "which" "magick")
                        :ignore-error-status t
                        :output 'string)
    (declare (ignore err))
    (cond
      ((= 0 ret)
       (list (str:trim out))))))

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
                                        (ignore-error-status nil)
                                        (async nil))
  "Wrapper for magick commands, in the future we might run this in-process"
  (restart-case
      (let* ((command (loop for str in command
                            if (pathnamep str)
                              collect (namestring str)
                            else
                              collect str))
             (command (append (magick-prefix) command)))

        (flet ((run ()
                 (call-with-semaphore
                  *semaphore*
                  (lambda ()
                    (uiop:run-program
                     command
                     :output output
                     :error-output error-output
                     :ignore-error-status ignore-error-status)))))
          (cond
            (async
             ;; this is the streaming functionality. In this case we
             ;; return a stream that's the output. This only works on
             ;; Lispworks
             #+lispworks
             (system:open-pipe command
                               :element-type '(unsigned-byte 8)
                               :direction :input)

             ;; Outside of lispworks, let's not try to complicate
             ;; things. Let's just save to a local temporary file, and
             ;; return that stream.
             #-lispworks
             (uiop:with-temporary-file (:pathname p)
               (setf output p)
               (run)
               (open run :direction :input :element-type '(unsigned-byte 8))))
            (t
             (run)))))
    (retry-run-magick ()
      (apply #'run-magick command args))))
