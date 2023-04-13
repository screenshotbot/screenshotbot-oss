;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/osicat-lw
  (:use #:cl))
(in-package :util/osicat-lw)

(defvar *embedded* nil)

(defun embed-osicat ()
  (fli:disconnect-module :libosicat :remove t)
  (fli:get-embedded-module :libosicat
                           (second
                            (asdf:output-files
                             'asdf:compile-op
                             (asdf:find-component
                              (asdf:find-component :osicat :posix)
                              "wrappers"))))
  (setf *embedded* t))

(defun load-osicat ()
  (log:info "Loading embedded osicat module")
  (fli:install-embedded-module :libosicat))

(unless (hcl:delivered-image-p)
 (lw:define-action "Delivery Actions" "Embed osicat"
   #'embed-osicat ))

(lw:define-action "When starting image" "Load embedded osicat"
  #'load-osicat)
