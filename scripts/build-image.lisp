;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package "CL-USER")

(require :asdf)
#+lispworks
(require "java-interface" )

(pushnew :asdf-unicode *features*)

#+lispworks
(progn
  (lw:set-default-character-element-type 'character))

(load "quicklisp/setup.lisp")
(ql:update-all-dists :prompt nil)

(pushnew :screenshotbot-oss *features*)

(push (pathname (format nil "~alocal-projects/" (uiop:getcwd))) ql:*local-project-directories*)
(push (pathname (format nil "~asrc/" (uiop:getcwd))) ql:*local-project-directories*)
(push (pathname (format nil "~athird-party/" (uiop:getcwd))) ql:*local-project-directories*)

(ql:quickload "log4cl")

;; required for some ASDF builds
(ql:quickload "uffi")

(log:info "*local-project-directories: ~S" ql:*local-project-directories*)
(ql:quickload "swank")
#+lispworks
(require "java-interface")

(swank-loader:init :load-contribs t)


;; make sure we have build asd
(push (pathname (format nil "~a/build-utils/" (namestring (uiop:getcwd))))
      asdf:*central-registry*)

(ql:register-local-projects)

#+lispworks
(load-all-patches)

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
  (let ((file (cadr ccl:*command-line-argument-list*)))
    (if
     (load file :verbose t)
     (loop
           (print (eval (read)))))))

#+ccl
(ccl:save-application "build/ccl-console"
                      :toplevel-function 'ccl-toplevel-function)
