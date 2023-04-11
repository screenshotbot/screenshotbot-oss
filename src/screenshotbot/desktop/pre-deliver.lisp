;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/desktop/pre-deliver
  (:use #:cl)
  (:import-from #:util/native-module
                #:load-module
                #:embed-module
                #:make-system-module)
  (:export
   #:call-pre-delivery
   #:install-modules))
(in-package :screenshotbot/desktop/pre-deliver)

(defvar *libwebp* (make-system-module :libwebp
                                      :file-name "libwebp.so"))
(defvar *libwebpmux* (make-system-module :libwebpmux
                                         :file-name "libwebpmux.so"))
(defvar *libwebpdemux* (make-system-module :libwebpdemux
                                         :file-name "libwebpdemux.so"))

(defvar *libpng* (make-system-module :libpng
                                     :file-name "libpng.so"))

(defvar *libgomp* (make-system-module :libgomp
                                      :file-name "libgomp.so.1"))

(defparameter *libs*
  ;; the order here matter!
  (list
   *libwebp*
   *libpng*
   *libwebpmux*
   *libwebpdemux*
   #+linux
   *libgomp*))

(defun ensure-no-pro-packages ()
  (loop for package in (list-all-packages)
        if (str:starts-with-p "SCREENSHOTBOT/PRO/" (package-name package))
          do
             (restart-case
                 (error "Package ~a is a bad package included in the system"
                        package)
               (ignore ()))))

(defun call-pre-delivery ()
  (ensure-no-pro-packages)
  (mapc #'embed-module *libs*)
  (screenshotbot/magick/magick-lw:embed-magick-native)
  (fli:disconnect-module
   'sqlite-ffi::sqlite3-lib :remove t))

(defun install-modules ()
  (mapc #'load-module *libs*))
