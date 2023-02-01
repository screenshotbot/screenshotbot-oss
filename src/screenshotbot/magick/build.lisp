;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/magick/build
  (:use :cl
   :asdf)
  (:export #:lib-source-file
           #:magick-cl-source-file))
(in-package :screenshotbot/magick/build)

(defclass lib-source-file (c-source-file)
  ())

(defclass magick-cl-source-file (cl-source-file)
  ())

(defparameter *library-file-dir*
  (make-pathname :name nil :type nil
                 :defaults *load-truename*))

(defun default-foreign-library-type ()
  "Returns string naming default library type for platform"
  #+(or win32 win64 cygwin windows) "dll"
  #+(or macosx darwin ccl-5.0) "dylib"
  #-(or win32 win64 cygwin windows macosx darwin ccl-5.0) "so"
)

(defmethod output-files ((o compile-op) (c lib-source-file))
  (let ((library-file-type
          (default-foreign-library-type)))
    (list (make-pathname :type library-file-type
                         :name (format nil "~a~a"
                                       (asdf:component-name c)
                                       (magick-lib-suffix))
                         :defaults (asdf:component-pathname c)))))

(defmethod perform ((o load-op) (c lib-source-file))
  t)

(defun guess-mac-magick-location ()
  (let ((dir "/opt/homebrew/Cellar/imagemagick/"))
    (loop for child in (uiop:subdirectories dir)
          for name = (car (last (pathname-directory child)))
          if (eql #\7 (elt name 0))
            return child)))

(defun windows-path ()
   (fad:pathname-directory-pathname
    (car (directory #P"C:/Program Files/ImageMagick-*-Q8/configure.xml"))))

(defun %path ()
  (destructuring-bind (name sep dir-sep)
      (if (uiop:os-windows-p)
          (list "Path" ";" "\\")
          (list "PATH" ":" "/"))
   (let* ((parts
            (str:split
             sep
             (uiop:getenv name))))
     (append
      (loop for part in parts
            if (str:ends-with-p
                dir-sep part)
              collect part
            else
              collect (format nil "~a~a" part dir-sep))
      (list
       ;; Mac Homebrew default location
       "/opt/homebrew/bin/"
       )
      (directory
       ;; Debian 11 default location. This returns the file itself,
       ;; but that's okay. The only caller of this will replace the
       ;; :name part of the pathname.
       "/usr/lib/x86_64-linux-gnu/ImageMagick-*/bin-q*/MagickWand-config")))))

(defun guess-include-dir ()
  (path:catdir (windows-path) "include/"))

(defun config-bin ()
  #+windows
  (error "MagickWand-config not supported on Windows")
  #-windows
  (namestring
   (let ((path (%path)))
     (loop for dir in path
           for file = (make-pathname
                       :name "MagickWand-config"
                       :type nil
                       :defaults (pathname dir))
           if (uiop:file-exists-p file)
             return file
           finally
              (warn "Could not find MagickWand-config in the PATH, or in the guessed locations: ~S" path)
              (return "MagickWand-config")))))

(defun magick-lib-suffix ()
  #+windows
  ".Q8"
  ;; I don't want to pull in cl-ppcre just for this...
  #-windows
  (let* ((haystack (uiop:run-program `(,(config-bin) "--libs")
                                     :output 'string))
         (needle "lMagickWand")
         (pos
           (search
            needle
            haystack)))

    (unless pos
      (error "Could not find lMagickWand in the output of MagickWand-config --libs: ~a" haystack))
    (let* ((str (subseq haystack (+ pos (length needle))))
           (end (search " " str)))
      (subseq str 0 end))))

(defun %string-trim (str)
  (String-trim '(#\Space #\Newline)
               str))

(defun magick-wand-config (arg)
  (%string-trim
   (uiop:run-program `(,(config-bin)
                       ,arg)
                     :output 'string)))

(defun init-features ()
  "If you're using these features, make sure that your files are compiled
 with the right suffix, so that they can coexist in the same
 repository build output. (see the example of magick-lw.lisp below)."
  #+windows
  (pushnew :magick-7 *features*)
  #-windows
  (let ((version (%string-trim (uiop:run-program `(,(config-bin)
                                                   "--version")
                                                 :output 'string))))
    (flet ((add-feature (add remove)
             (setf *features* (remove remove *features*))
             (pushnew add *features*)))
     (ecase (elt version 0)
       (#\6
        (add-feature :magick-6 :magick-7))
       (#\7
        (add-feature :magick-7 :magick-6))))))

(init-features)

(defmethod perform ((o compile-op) (c lib-source-file))
  (uiop:run-program
   (format nil"gcc -std=c99 -fPIC -shared ~a ~a -I -Werror -O2 -Wall ~a -o ~a"

           (uiop:escape-shell-command (namestring
                                       (component-pathname c)))
           #-windows
           (magick-wand-config "--cflags")
           #-windows
           (magick-wand-config "--ldflags")
           #+windows
           (format nil "-DQUANTUM_DEPTH=8 -DHDRI=0 -I~a" (uiop:escape-shell-token (namestring (guess-include-dir))))
           #+windows
           (format nil " -lCORE_RL_MagickWand_ -lCORE_RL_MagickCore_ -L~a" (uiop:escape-shell-token (namestring (windows-path))))
           (namestring (car (output-files o c))))
   :output *standard-output*
   :error-output *error-output*))

(defmethod output-files ((o compile-op) (c magick-cl-source-file))
  (let ((old (car (call-next-method))))
    (list
     (make-pathname
      :name (format nil "~a~a"
                    (component-name c)
                    (magick-lib-suffix))
      :defaults old))))
