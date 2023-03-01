;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#+lispworks
(require "java-interface" )

;; For SBCL, if you don't have SBCL_HOME set, then we won't be able to require this later.
#+sbcl
(require "sb-introspect")

#+lispworks
(progn
  (lw:set-default-character-element-type 'character))

(when (probe-file "scripts/asdf.lisp")
  (format t "Compiling asdf..~%")
  (let ((output (compile-file "scripts/asdf.lisp" :verbose nil :print nil)))
    (load output))
  (provide "asdf"))

(require "asdf")

#+sbcl
(require "sb-sprof")

#+nil
(push (pathname (format nil "~a/local-projects/poiu/" (namestring (uiop:getcwd))))
      asdf:*central-registry*)


(defparameter *cwd* (merge-pathnames
               *default-pathname-defaults*
               (uiop:getcwd)))

(asdf:initialize-output-translations `(:output-translations
                                       :inherit-configuration
                                       (,(namestring *cwd*)
                                         ,(format nil "~abuild/asdf-cache/~a/" *cwd*
                                                  (uiop:implementation-identifier)))))

#+sbcl
(progn
  (require :sb-rotate-byte)
  (require :sb-cltl2)
  (asdf:register-preloaded-system :sb-rotate-byte)
  (asdf:register-preloaded-system :sb-cltl2))


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

(defun %read-version (file)
  (let ((key "version: "))
   (loop for line in (uiop:read-file-lines file)
         if (string= key line :end2 (length key))
           return (subseq line (length key)))))


(defun init-quicklisp ()
  (let ((version (%read-version "quicklisp/dists/quicklisp/distinfo.txt")))
    (let ((quicklisp-loc (ensure-directories-exist
                          (merge-pathnames
                           (format nil "build/quicklisp/~a/" version)
                           *cwd*)))
          (src (merge-pathnames
                "quicklisp/"
                *cwd*)))
      (flet ((safe-copy-file (path &optional (dest path))
               (let ((src (merge-pathnames
                           path
                           "quicklisp/"))
                     (dest (merge-pathnames
                            dest
                            quicklisp-loc)))
                 (format t "Copying: ~a to ~a~%" src dest)

                 (when (equal src dest)
                   (error "Trying to overwrite the same file"))
                 (unless (uiop:file-exists-p dest)
                   (uiop:copy-file
                    src
                    (ensure-directories-exist
                     dest))))))
        (loop for name in
                       (append (directory
                                (merge-pathnames
                                 "quicklisp/quicklisp/*.lisp"
                                 *cwd*))
                               (directory
                                (merge-pathnames
                                 "quicklisp/quicklisp/*.asd"
                                 *cwd*)))
              do (safe-copy-file name
                                 (format nil "quicklisp/~a.~a"
                                         (pathname-name name)
                                         (pathname-type name))))
        (loop for name in (directory
                           (merge-pathnames
                            "quicklisp/*.lisp"
                            *cwd*))
              do (safe-copy-file name
                                 (format nil "~a.lisp"
                                         (pathname-name name))))
        (safe-copy-file "setup.lisp")
        (safe-copy-file "quicklisp/version.txt")
        (safe-copy-file "dists/quicklisp/distinfo.txt")
        (safe-copy-file "dists/quicklisp/enabled.txt")
        (safe-copy-file "dists/quicklisp/preference.txt"))
      (load (merge-pathnames
             "setup.lisp"
             quicklisp-loc)))))

(init-quicklisp)

#+nil
(ql:update-all-dists :prompt nil)

(pushnew :screenshotbot-oss *features*)

#-screenshotbot-oss
(push (pathname (format nil "~alocal-projects/" *cwd*) ) ql:*local-project-directories*)
(push (pathname (format nil "~asrc/" *cwd*)) ql:*local-project-directories*)
(push (pathname (format nil "~athird-party/" *cwd*)) ql:*local-project-directories*)

(defun maybe-configure-proxy ()
  (let ((proxy (uiop:getenv "HTTP_PROXY")))
    (when (and proxy (> (length proxy) 0))
      (setf ql:*proxy-url* proxy))))

(maybe-configure-proxy)


(ql:quickload "log4cl")

;; for SLY
(ql:quickload "flexi-streams")

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
