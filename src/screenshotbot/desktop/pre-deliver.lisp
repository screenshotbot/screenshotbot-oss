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

(defun call-pre-delivery ()
  (embed-module *libjbig*)
  (screenshotbot/magick/magick-lw:embed-magick-native)
  (fli:disconnect-module
   :libosicat :remove t)
  (fli:disconnect-module
   'sqlite-ffi::sqlite3-lib :remove t))

(defun install-modules ()
  (load-module *libjbig*))
