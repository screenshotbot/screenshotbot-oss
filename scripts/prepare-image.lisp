;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :cl-user)

#+lispworks
(require "java-interface" )

;; For SBCL, if you don't have SBCL_HOME set, then we won't be able to require this later.
#+sbcl
(require "sb-introspect")

#+lispworks
(progn
  (lw:set-default-character-element-type 'character))

(when (probe-file ".jipr")
  (push :jipr *features*))

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

(defvar *asdf-root-guesser* nil)

(defparameter *cwd* (merge-pathnames
               *default-pathname-defaults*
               (uiop:getcwd)))

(defun update-output-translations (root)
  "This function is called dynamically from deliver-utils/common.lisp!"
  (asdf:initialize-output-translations
   `(:output-translations
     :inherit-configuration
     (,(namestring root)
      ,(format nil "~abuild/asdf-cache/~a~a/" root
               (uiop:implementation-identifier)
               #+android-delivery
               "-android"
               #-android-delivery
               "")))))
(compile 'update-output-translations)
(update-output-translations *cwd*)

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



(defun update-project-directories (cwd)
  (flet ((push-src-dir (name)
           (let ((dir (pathname (format nil "~a~a/" cwd name))))
             (when (probe-file dir)
               (push dir ql:*local-project-directories*)))))
    #-screenshotbot-oss
    (push-src-dir "local-projects")
    (push-src-dir "src")
    (push-src-dir "third-party")
    (push-src-dir "lisp")))
(compile 'update-project-directories)

(defun update-root (cwd)
  (update-output-translations cwd)
  (update-project-directories cwd))
(compile 'update-root)

(update-project-directories *cwd*)

(defvar *initial-path* *cwd*
  "The CWD before the image was saved")

(defun fix-absolute-path (path)
  (let* ((initial-path *initial-path*)
         (cwd (uiop:getcwd))
         (path (pathname path))
         (initial-parts (pathname-directory initial-path))
         (parts (pathname-directory cwd)))
    (cond
      ((and
        (> (length (pathname-directory path))
           (length initial-parts))
        (equalp
         (subseq (pathname-directory path) 0 (length initial-parts))
         initial-parts))
       (make-pathname
        :directory
        (append
         parts
         (subseq (pathname-directory path) (length initial-parts)))
        :defaults path))
      (t
       path))))
(compile 'fix-absolute-path)

(defmacro fix-paths-in (place)
  `(setf ,place
         (mapcar #'fix-absolute-path ,place)))

(setf asdf::*base-build-directory* *cwd*)

(defun fix-absolute-registry-paths ()
  (when
      #-lispworks t
      #+lispworks
      (or
         (not (boundp 'lw:*delivery-level*))
         (= 0 lw:*delivery-level*))
    (setf asdf::*base-build-directory* (uiop:getcwd))
    (fix-paths-in asdf:*central-registry*)
    (fix-paths-in ql:*local-project-directories*)
    (update-output-translations (uiop:getcwd))
    (setf ql-setup:*quicklisp-home*
          (fix-absolute-path ql-setup:*quicklisp-home*))
    (quicklisp:setup)
    (ql:register-local-projects)))

(compile 'fix-absolute-registry-paths)

#+lispworks
(lw:define-action "When starting image" "Fix absolute registry paths"
  #'fix-absolute-registry-paths)


(defun maybe-asdf-prepare ()
  (when *asdf-root-guesser*
    (update-root (funcall *asdf-root-guesser*))))

(compile 'maybe-asdf-prepare)

#+lispworks
(lw:define-action "When starting image" "Re-prepare asdf"
  #'maybe-asdf-prepare)

(defun unprepare-asdf (root-guesser)
  "This function is called dynamically from deliver-utils/common.lisp!"
  (setf *asdf-root-guesser* root-guesser))

(defun maybe-configure-proxy ()
  (let ((proxy (uiop:getenv "HTTP_PROXY")))
    (when (and proxy (> (length proxy) 0))
      (setf ql:*proxy-url* proxy))))

(maybe-configure-proxy)


;; for SLY
(ql:quickload "flexi-streams")

#+sbcl ;; not sure why I need this, I didn't debug in detail
(ql:quickload "prove-asdf")

(ql:quickload :documentation-utils)

;;(log:info "*local-project-directories: ~S" ql:*local-project-directories*)

#+lispworks
(require "java-interface")

(ql:quickload :cl-ppcre) ;; used by sdk.deliver

;; make sure we have build asd
#+nil
(push (pathname (format nil "~a/build-utils/" *cwd*))
      asdf:*central-registry*)
(ql:register-local-projects)
