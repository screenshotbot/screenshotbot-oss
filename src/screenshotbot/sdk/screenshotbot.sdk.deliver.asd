;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot.sdk.deliver
  (:use :cl :asdf))
(in-package :screenshotbot.sdk.deliver)

(defparameter *library-file-dir*
  (make-pathname :name nil :type nil
                 :defaults *load-truename*))

(defclass deliver-script (source-file)
  ((type :initform "lisp")))

(defmethod output-files ((o compile-op) (s deliver-script))
  (let ((output (make-pathname :name (funcall (find-symbol "REGEX-REPLACE-ALL" "CL-PPCRE")
                                 "^deliver-" (component-name s) "")
                  :type nil
                  :defaults *library-file-dir*)))
    (list
     output
     #+darwin
     (make-pathname :type "lwheap"
                    :defaults output))))

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
                   :defaults *library-file-dir*))))

(defmethod perform ((o load-op) (m makeself-component))
  t)

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
                      :output :interactive
                      :error-output :interactive)))

(defmethod perform ((o compile-op) (m makeself-component))
  (eval
   `(let ((o ,o) (m ,m))
        (,(find-symbol "WITH-TMPDIR" "TMPDIR") (tmpdir)
         (do-perform tmpdir o m)))))

(defmethod component-pathname ((m makeself-component))
  (call-next-method))

(defun lw ()
  (or
   #+(and :x86-64 darwin)
   "build/lw-console-8-0-0x86_64"
   #+(or :linux (and :arm64 :darwin))
   (format nil "~a/builds/web/build/lw-console-8-0-0"
           (uiop:getenv "HOME"))
   (error "unsupported platform image")))

(defmethod perform ((o compile-op) (s deliver-script))
  (uiop:run-program (list (lw)
                          "-build"
                          (namestring
                           (merge-pathnames (format nil "~a.lisp" (component-name s))
                                            *library-file-dir*))
                          (namestring
                           (car (output-files o s))))
                    :output :interactive
                    :error-output :interactive))

(defsystem :screenshotbot.sdk.deliver
  :author "Arnold Noronha <arnold@screenshotbot.io>"
  :license "Mozilla Public License, v 2.0"
  :defsystem-depends-on (:cl-ppcre #+nil :tmpdir)
  :depends-on (:screenshotbot.sdk)
  :components ((deliver-script "deliver-sdk")
               (makeself-component "installer"
                                   :depends-on ("deliver-sdk")
                                   :type "sh"
                                   :label "screenshotbot-installer"
                                   :archive ("deliver-sdk"
                                             "installer")
                                   :startup-component "installer")))
