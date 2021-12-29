;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#+lispworks
(require "java-interface" )

#+lispworks
(progn
  (lw:set-default-character-element-type 'character))

(require "asdf")

#+nil
(push (pathname (format nil "~a/local-projects/poiu/" (namestring (uiop:getcwd))))
      asdf:*central-registry*)


(defvar *cwd* (merge-pathnames
               *default-pathname-defaults*
               (uiop:getcwd)))

(asdf:initialize-output-translations `(:output-translations
                                       :inherit-configuration
                                       (,(namestring *cwd*)
                                         ,(format nil "~abuild/asdf-cache/~a/" *cwd*
                                                  (uiop:implementation-identifier)))))


#+lispworks
(defun use-utf-8-for-all-lisp-files (pathname ext-format-spec first-byte max-extent)
  (cond
    ((equal "lisp" (pathname-type pathname))
     :utf-8)
    (t ext-format-spec)))

#+lispworks
(compile 'use-utf-8-for-all-lisp-files)

#+lispworks
(push 'use-utf-8-for-all-lisp-files system:*file-encoding-detection-algorithm*)

(load "quicklisp/setup.lisp")

#+nil
(ql:update-all-dists :prompt nil)

(pushnew :screenshotbot-oss *features*)

#-screenshotbot-oss
(push (pathname (format nil "~alocal-projects/" *cwd*) ) ql:*local-project-directories*)
(push (pathname (format nil "~asrc/" *cwd*)) ql:*local-project-directories*)
(push (pathname (format nil "~athird-party/" *cwd*)) ql:*local-project-directories*)


(ql:quickload "log4cl")

#+sbcl ;; not sure why I need this, I didn't debug in detail
(ql:quickload "prove-asdf")

(ql:quickload :documentation-utils)

(log:info "*local-project-directories: ~S" ql:*local-project-directories*)

#+lispworks
(require "java-interface")

(ql:quickload :cl-ppcre) ;; used by sdk.deliver

;; make sure we have build asd
#+nil
(push (pathname (format nil "~a/build-utils/" *cwd*))
      asdf:*central-registry*)

(ql:register-local-projects)
