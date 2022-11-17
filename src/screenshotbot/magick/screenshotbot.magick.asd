;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.
(defpackage :screenshotbot-magick-system
  (:use :cl
   :asdf))
(in-package :screenshotbot-magick-system)

(defclass lib-source-file (c-source-file)
  ())

(defparameter *library-file-dir*
  (make-pathname :name nil :type nil
                 :defaults *load-truename*))

(defun default-foreign-library-type ()
  "Returns string naming default library type for platform"
  #+(or win32 win64 cygwin mswindows windows) "dll"
  #+(or macosx darwin ccl-5.0) "dylib"
  #-(or win32 win64 cygwin mswindows windows macosx darwin ccl-5.0) "so"
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

(defun magick-lib-suffix ()
  ;; I don't want to pull in cl-ppcre just for this...
  (let* ((haystack (uiop:run-program `("MagickWand-config" "--libs")
                                     :output 'string))
         (needle "lMagickWand")
         (pos
           (search
            needle
            haystack))
         (str (subseq haystack (+ pos (length needle))))
         (end (search " " str)))
    (subseq str 0 end)))

(defun %string-trim (str)
  (String-trim '(#\Space #\Newline)
               str))

(defun magick-wand-config (arg)
  (%string-trim
   (uiop:run-program `("MagickWand-config"
                       ,arg)
                     :output 'string)))

(defun init-features ()
  (let ((version (%string-trim (uiop:run-program `("MagickWand-config"
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
   (format nil"gcc -shared ~a ~a -I -Werror -O2 -Wall ~a -o ~a"

           (uiop:escape-shell-command (namestring
                                       (component-pathname c)))
           (magick-wand-config "--cflags")
           (magick-wand-config "--ldflags")
           (namestring (car (output-files o c))))
   :output *standard-output*
   :error-output *error-output*))



(defsystem :screenshotbot.magick
  :serial t
  :depends-on ((:feature (:not :lispworks) :util/fake-fli)
               :easy-macros
               :screenshotbot/events
               :alexandria)
  :components ((:file "magick")
               (lib-source-file "magick-native")
               (:file "memory" :if-feature :lispworks)
               (:file "ffi-7" :if-feature :magick-7)
               (:file "ffi-6" :if-feature :magick-6)
               (:file "magick-lw")))

(defsystem :screenshotbot.magick/tests
  :serial t
  :depends-on (:util/fiveam
               :fiveam-matchers
               :util/digests
               :screenshotbot.magick)
  :components ((:file "test-magick-lw")
               (:file "test-memory" :if-feature :lispworks)))
