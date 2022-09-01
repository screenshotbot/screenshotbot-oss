;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/magick/magick
  (:nicknames :screenshotbot/magick)
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:run-magick
   #:compare-image-files
   #:magick
   #:*magick*
   #:convert-to-lossless-webp
   #:ping-image-dimensions
   #:with-magick-gatekeeper))
(in-package :screenshotbot/magick/magick)

(defclass abstract-magick ()
  ())

(defclass magick-cli (abstract-magick)
  ())

(defvar *magick* (make-instance 'magick-cli))

(defun magick ()
  *magick*)

(defvar *semaphore* (bt:make-semaphore
                     :name "magick"
                     :count (serapeum:count-cpus)))

(defun call-with-semaphore (sem fn)
  (unwind-protect
       (progn
         (bt:wait-on-semaphore sem :timeout 60)
         (funcall fn))
    (bt:signal-semaphore sem)))

(defun magick-prefix-uncached ()
  (cond
   ((uiop:os-unix-p)
    (multiple-value-bind (out err ret)
        (uiop:run-program (list "which" "magick")
                          :ignore-error-status t
                          :output 'string)
      (declare (ignore err))
      (cond
       ((= 0 ret)
        (list (str:trim out))))))
   ((uiop:os-windows-p)
    (list "C:\\Program Files\\ImageMagick-7.1.0-Q8\\magick"))
   (t
    (error "Unsupported impl"))))

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

(defun default-limits ()
  (list
   "-limit" "memory" "3MB"
   "-limit" "map" "3MB"
   "-limit" "disk" "1000MB"))

(defun run-magick (command &rest args &key (error-output t) (output t)
                                        (ignore-error-status nil)
                                        (async nil)
                                        (lock t))
  "Wrapper for magick commands, in the future we might run this in-process"
  (restart-case
      (let* ((command (loop for str in command
                            if (pathnamep str)
                              collect (namestring str)
                            else
                              collect str))
             (command `(,(car command)
                        ,@(default-limits)
                        ,@(cdr command)))
             (command (append (magick-prefix) command)))

        (labels ((actually-run ()
                   (uiop:run-program
                    command
                    :output output
                    :error-output error-output
                    :ignore-error-status ignore-error-status))
                 (run ()
                   (cond
                     (lock
                      (call-with-semaphore
                       *semaphore*
                       #'actually-run))
                     (t
                      (actually-run)))))
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
               (open p :direction :input :element-type '(unsigned-byte 8))))
            (t
             (run)))))
    (retry-run-magick ()
      (apply #'run-magick command args))))


(defmethod compare-image-files ((magick magick-cli) file1 file2)
  (multiple-value-bind (out err ret)
      (run-magick
       (list "compare" "-metric" "RMSE" file1 file2 "null:")
       :error-output 'string
       :ignore-error-status t
       :lock nil)
    (declare (ignore out))
    (and (= 0 ret)
         (string= "0 (0)" (str:trim err)))))

(defmacro with-magick-gatekeeper (()  &body body)
  `(call-with-semaphore
    *semaphore*
    (lambda ()
      ,@body)))

(defmethod compare-image-files :around ((magick abstract-magick) file1 file2)
  (with-magick-gatekeeper ()
    (call-next-method)))


(defmethod convert-to-lossless-webp ((magick magick-cli) file1 output)
  (run-magick
   (list
    "convert" file1 "-strip" "-define" "webp:lossless=true" output)
   :lock nil))

(defmethod convert-to-lossless-webp ((magick abstract-magick) file1 output)
  (assert (equal "webp" (pathname-type output)))
  (with-magick-gatekeeper ()
    (call-next-method)))

(defmethod ping-image-dimensions ((magick magick-cli) file)
  (let ((res (run-magick
              (list "identify" "-format" "%w %h" file)
              :output 'string)))
    (mapcar #'parse-integer
              (str:split " " res))))
