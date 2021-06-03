;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package "CL-USER")

(let ((output (compile-file "scripts/asdf.lisp")))
  (load output))

(provide "asdf")

(asdf:initialize-output-translations `(:output-translations
                                       :inherit-configuration
                                       (,(namestring (uiop:getcwd))
                                         ,(format nil "~abuild/asdf-cache/~a/" (uiop:getcwd)
                                                  (uiop:implementation-identifier)))))

#+lispworks
(require "java-interface" )

(defun use-utf-8-for-all-lisp-files (pathname ext-format-spec first-byte max-extent)
  (cond
    ((equal "lisp" (pathname-type pathname))
     :utf-8)
    (t ext-format-spec)))

#+lispworks
(push 'use-utf-8-for-all-lisp-files system:*file-encoding-detection-algorithm*)

#+lispworks
(progn
  (lw:set-default-character-element-type 'character))

(load "quicklisp/setup.lisp")

#+nil
(ql:update-all-dists :prompt nil)

(pushnew :screenshotbot-oss *features*)

(push (pathname (format nil "~alocal-projects/" (uiop:getcwd))) ql:*local-project-directories*)
(push (pathname (format nil "~asrc/" (uiop:getcwd))) ql:*local-project-directories*)
(push (pathname (format nil "~athird-party/" (uiop:getcwd))) ql:*local-project-directories*)

(ql:quickload "azula")

(azula:config :root #P "./")

(ql:quickload "log4cl")

(log:info "*local-project-directories: ~S" ql:*local-project-directories*)

(load "third-party/slime/swank-loader.lisp")
(setf swank-loader:*fasl-directory* (format nil "~abuild/slime-fasls/~a/" (uiop:getcwd)
                                            (uiop:implementation-identifier)))
(push 'swank-indentation swank-loader::*contribs*)
(swank-loader:init :load-contribs t)

#+lispworks
(require "java-interface")

(ql:quickload :cl-ppcre) ;; used by sdk.deliver

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
