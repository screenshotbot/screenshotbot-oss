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

(defvar *libjbig* (make-system-module :libjbig
                                      :file-name "libjbig.so"))

(defvar *liblcms2* (make-system-module :liblcms2
                                       :file-name "liblcms2.so"))

(defvar *libtiff* (make-system-module :libtiff
                                      :file-name "libtiff.so"))

(defvar *libwebp* (make-system-module :libwebp
                                      :file-name "libwebp.so"))
(defvar *libwebpmux* (make-system-module :libwebpmux
                                         :file-name "libwebpmux.so"))
(defvar *libwebpdemux* (make-system-module :libwebpdemux
                                         :file-name "libwebpdemux.so"))

(defvar *libpng* (make-system-module :libpng
                                     :file-name "libpng.so"))

(defvar *libjpeg* (make-system-module :libjpeg
                                      :file-name "libjpeg.so"))

(defvar *libdeflate* (make-system-module :libdeflate
                                         :file-name "libdeflate.so"))

(defvar *liblqr* (make-system-module :liblqr
                                     :file-name "liblqr-1.so"))

(defvar *libglib-2.0* (make-system-module :libglib-2.0
                                          :file-name "libglib-2.0.so"))

(defvar *libgomp* (make-system-module :libgomp
                                      :file-name "libgomp.so.1"))

(defvar *libdjvulibre* (make-system-module :libdjvulibre
                                           :file-name "libdjvulibre.so"))


(defparameter *libs*
  ;; the order here matter!
  (list
   *libwebp*
   *libjpeg*
   *libdeflate*
   *libjbig*
   *libtiff*
   *liblcms2*
   *libpng*
   *libglib-2.0*
   *liblqr*
   *libdjvulibre*
   ;; good ones:
   *libwebpmux*
   *libwebpdemux*
   *libgomp*))

(defun call-pre-delivery ()
  (mapc #'embed-module *libs*)
  (screenshotbot/magick/magick-lw:embed-magick-native)
  (fli:disconnect-module
   :libosicat :remove t)
  (fli:disconnect-module
   'sqlite-ffi::sqlite3-lib :remove t))

(defun install-modules ()
  (mapc #'load-module *libs*))
