;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package "CL-USER")

(load "scripts/prepare-image.lisp")

#+lispworks
(load-all-patches)

(defun image-load-hook ()
  (format t "Running image load hooks~%")
  (ql:quickload :deadbeef)
  (funcall (find-symbol "REGISTER-EXTERNAL" "DEADBEEF")
           "https://github.com/tdrhq/stripe"
           "6b91ee9bcbffe81f887a0edddd1b182951cd02cf")
  (funcall (find-symbol "PREPARE-EXTERNALS" "DEADBEEF")
           "build/deadbeef/")
  (format t "Ran image load hooks~%"))

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
        (image-load-hook))))


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
