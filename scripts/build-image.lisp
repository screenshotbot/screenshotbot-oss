;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package "CL-USER")

(load "scripts/prepare-image.lisp")

#+lispworks
(load-all-patches)

(defvar *image-load-hook-contents* (uiop:read-file-string "scripts/init.lisp"))
(defvar *hook-loaded-p* nil)

(defun image-load-hook ()
  ;; If we used this image to deliver another image, we don't
  ;; want to load the same hook twice
  (unless *hook-loaded-p*
    (load (make-string-input-stream *image-load-hook-contents*))
    (setf *hook-loaed-p* t)))

(compile 'image-load-hook)

#+sbcl
(pushnew 'image-load-hook sb-ext:*init-hooks*)


#+lispworks
(lw:define-action "When starting image" "Call image load hook"
  #'(lambda ()
      (handler-bind ((error
                       (lambda (e)
                         (declare (ignore e))
                         (dbg:output-backtrace :verbose)
                         (format t "Could not load init~%")
                         (uiop:quit 1))))
        (format t "Running image load hooks~%")
        (image-load-hook)
        (format t "Completed image load hooks~%"))))


#+lispworks
(let ((output "build/lw-console"))
  (delete-file output)
  (save-image output
              :console t
             :multiprocessing t
             :environment nil))

#+sbcl
(sb-ext:save-lisp-and-die "build/sbcl-console"
                          :executable t)

#+ccl
(defun ccl-toplevel-function ()
  (image-load-hook)
  (let ((file (cadr ccl:*command-line-argument-list*)))
    (if file
     (load file :verbose t)
     (loop
           (print (eval (read)))))))


#+ccl
(ccl:save-application "build/ccl-console"
                      :toplevel-function 'ccl-toplevel-function)
