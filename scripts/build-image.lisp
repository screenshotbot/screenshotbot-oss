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
  ;; On MacOS, the TMPDIR variable can change between sessions.
  (uiop:setup-temporary-directory)

  #-sbcl
  (log4cl::init-hook)

  ;; If we used this image to deliver another image, we don't
  ;; want to load the same hook twice
  (unless *hook-loaded-p*
    (load (make-string-input-stream *image-load-hook-contents*))
    (setf *hook-loaded-p* t)))

(compile 'image-load-hook)

#+sbcl
(pushnew 'image-load-hook sb-ext:*init-hooks*)

#+lispworks
(defun lw-image-load-hook ()
  (handler-bind ((error
                   (lambda (e)
                     (declare (ignore e))
                     (dbg:output-backtrace :verbose)
                     (format t "Could not load init~%")
                     (uiop:quit 1))))
    (image-load-hook)))

#+lispworks
(compile 'lw-image-load-hook)

#+lispworks
(lw:define-action "When starting image" "Call image load hook"
  #'lw-image-load-hook)

(format t "Got command line arguments: ~S" (uiop:raw-command-line-arguments))

#-sbcl
(log4cl::save-hook)

#+lispworks
(flet ((build ()
         (let ((output (car (last (uiop:raw-command-line-arguments)))))
           (delete-file output)
           (save-image output
                       :console t
                       :multiprocessing t
                       :environment nil))))
  #-darwin
  (build)
  #+darwin
  (cond
    ((hcl:building-universal-intermediate-p)
     (build))
    (t
     (hcl:save-universal-from-script "scripts/build-image.lisp"))))


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
