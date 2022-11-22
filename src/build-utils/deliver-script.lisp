;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :build-utils/deliver-script
  (:use #:cl
        #:asdf)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:deliver-so-script
   #:makeself-component
   #:deliver-script
   #:default-deliver))
(in-package :build-utils/deliver-script)

(defclass deliver-script (source-file)
  ((type :initform "lisp")))

(defmethod output-type ((self deliver-script))
  #-mswindows nil #+mswindows "exe")

(defmethod output-files ((o compile-op) (s deliver-script))
  (let ((output (make-pathname :name (cl-ppcre:regex-replace-all
                                      "^deliver-" (component-name s) "")
                               :type (output-type s)
                               :defaults (component-pathname s))))
    (list
     output
     #+darwin
     (make-pathname :type "lwheap"
                    :defaults output))))

(defclass deliver-so-script (source-file)
  ((type :initform "lisp")))

(defmethod output-type ((self deliver-so-script))
  #+mswindows
  "dll"
  #-mswindows
  "so")

(defmethod perform ((o load-op) (s deliver-script))
  t)

(defclass makeself-component (source-file)
  ((label :initarg :label)
   (type :initform "sh")
   (archive :initarg :archive)
   (startup-component :initarg :startup-component)))


(defmethod output-files ((o compile-op) (m makeself-component))
  (let ((name (component-name m)))
    (list
     (make-pathname :name name
                    :type nil
                    :defaults (component-pathname m)))))

(defmethod perform ((o load-op) (m makeself-component))
  t)

(defun safe-delete-file (x)
  (when (uiop:file-exists-p x)
    (delete-file x)))

(defun do-perform (tmpdir o m)
  (with-slots (archive label startup-component license) m
    (loop for component in (cons m archive)
          do
             (let ((component (find-component
                               (component-parent m)
                               component)))
               (let* ((froms (if (or (typep component 'static-file)
                                    (eq m component))
                                (input-files 'compile-op component)
                                (output-files 'compile-op component))))
                 (dolist (from froms)
                   (let ((to (make-pathname :defaults  from
                                            :directory (pathname-directory tmpdir))))
                     (uiop:copy-file from to)
                     (uiop:run-program (list "chmod" "a+x" (namestring to))))))))

    (uiop:run-program (list #-darwin "makeself"
                            #+darwin "/opt/homebrew/bin/makeself"
                            "--nox11"
                            (namestring tmpdir)
                            (namestring (output-file o m))
                            label
                            (let ((pathname (output-file
                                              'compile-op
                                              m)))
                             (format nil "./~a.~a"
                                     (pathname-name pathname)
                                     "sh")))
                      :output t
                      :error-output t)))

(defmethod perform ((o compile-op) (m makeself-component))
  (eval
   `(let ((o ,o) (m ,m))
        (,(find-symbol "WITH-TMPDIR" "TMPDIR") (tmpdir)
         (do-perform tmpdir o m)))))

(defun lw ()
  (or
   #+(and :x86-64 darwin)
   "build/lw-console-8-0-0x86_64"
   #+(or :linux (and :arm64 :darwin))
   (format nil "~a/builds/web/build/lw-console-8-0-0"
           (uiop:getenv "HOME"))
   #+mswindows
   ".\\build\\lw-console-8-0-0"
   #+nil
   (namestring (uiop:ensure-absolute-pathname
                (pathname "build/lw-console-8-0-0.exe")))
   (error "unsupported platform image")))

(defmethod component-pathname ((m makeself-component))
  (call-next-method))

(defmethod perform ((o compile-op) (s deliver-script))
  (uiop:run-program (append (cond
                            #+lispworks
                            (t (list (lw) "-build"))
                            #+sbcl
                            (t (list
                                (namestring
                                 (make-pathname
                                  ;; win32 is just what SBCL calls it,
                                  ;; it seems to be true on 64 bit
                                  ;; too.
                                  #+win32  :type #+win32 "exe"
                                  :defaults
                                  #P"build/sbcl-console"))

                                "--script"))
                            (t (error "Unimplemented for CL implementation")))

                          (list
                           (namestring
                            (asdf:component-pathname s))
                           (namestring
                            (car (output-files o s)))))
                    :directory (asdf:system-relative-pathname :build-utils "../../")
                    :output t
                    :error-output t))


(defun default-deliver (fn-name output-file deliver-level &rest args)
  #-lispworks
  (declare (ignore args deliver-level))

  #+ccl
  (error "unimplemented")

  #+sbcl
  (sb-ext:save-lisp-and-die
   output-file
   :toplevel fn-name
   :executable t
   :save-runtime-options t)

  #+lispworks
  (apply #'lw:deliver fn-name
         output-file
         deliver-level
         (append
          args
          (list
           :keep-function-name t
           #+mswindows :console #+mswindows :init
           #+mswindows :startup-bitmap-file #+mswindows nil
           :keep-debug-mode t
           :keep-pretty-printer t
           :keep-clos-object-printing t
           :keep-lisp-reader t
           ;; temporary: get the build green
           :keep-eval t
           :keep-symbols `(system:pipe-exit-status
                           dspec:find-dspec-locations)
           :packages-to-keep-symbol-names :all
           :multiprocessing t)))

  (uiop:quit))
