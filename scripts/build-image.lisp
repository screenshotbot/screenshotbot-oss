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
  (ql:quickload :deadbeef))

(compile 'image-load-hook)

#+sbcl
(pushnew 'image-load-hook sb-ext:*init-hooks*)


#+lispworks
(lw:define-action "When starting image" "Call image load hook"
  #'(lambda ()
      (image-load-hook)))


#+lispworks
(save-image "build/lw-console"
            :console t
            :multiprocessing t
            :environment nil)

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
