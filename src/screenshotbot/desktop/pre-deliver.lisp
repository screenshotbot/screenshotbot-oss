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
  (fli:disconnect-module
   'sqlite-ffi::sqlite3-lib :remove t))

(defun install-modules ())
