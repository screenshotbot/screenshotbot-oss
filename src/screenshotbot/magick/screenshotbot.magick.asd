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
                         :defaults (asdf:component-pathname c)))))

(defmethod perform ((o load-op) (c lib-source-file))
  t)

(defun guess-mac-magick-location ()
  (let ((dir "/opt/homebrew/Cellar/imagemagick/"))
    (loop for child in (uiop:subdirectories dir)
          for name = (car (last (pathname-directory child)))
          if (eql #\7 (elt name 0))
            return child)))

(defun magick-wand-config (&optional ldflags-p)
  (string-trim '(#\Space #\Newline)
   (uiop:run-program `("MagickWand-config"
                       ,(if ldflags-p "--ldflags" "--cflags"))
                     :output 'string)))


(defmethod perform ((o compile-op) (c lib-source-file))
  (uiop:run-program
   (format nil"gcc -shared ~a ~a -I -Werror -O2 -Wall ~a -o ~a"

           (uiop:escape-shell-command (namestring
                                       (component-pathname c)))
           (magick-wand-config)
           (magick-wand-config t)
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
               (:file "magick-lw")))

(defsystem :screenshotbot.magick/tests
  :serial t
  :depends-on (:util/fiveam
               :fiveam-matchers
               :util/digests
               :screenshotbot.magick)
  :components ((:file "test-magick-lw")
               (:file "test-memory" :if-feature :lispworks)))
