;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/ui/assets
  (:use #:cl)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:export
   #:*asset-list*
   #:ensure-asset
   #:define-css
   #:handle-asdf-output
   #:%handle-asdf-output))
(in-package :core/ui/assets)

(defvar *lock* (bt:make-lock "assets-lock"))

(defvar *asset-list* nil
  "This is used in the desktop app, to precompute assets into memory.

In other situations, you can use this to iterate through all the
possible assets.")

(defun ensure-asset (system)
  "Just adds the system to the list of asset-cache. Note that this not
cause the asset to be immediately compiled."
  (pushnew system *asset-list*))

(defmacro handle-asdf-output (op component &optional (output-num 0))
  (let ((output-files (eval `(util:relative-output-files ,op (asdf:find-component ,component nil)))))
    `(%handle-asdf-output
      *installation*
      ,op
      ,component
      ',output-files
      ,output-num)))

(defmacro define-css (class uri asdf-target)
  "Defines a CSS asset for the acceptor of type CLASS"
  (let ((map-uri (format nil "~a.map" uri)))
    `(progn
       (ensure-asset ,asdf-target)
       (hex:def-clos-dispatch ((self ,class) ,uri) ()
         (setf (hunchentoot:content-type*)  "text/css; charset=utf-8")
         (handle-asdf-output 'asdf:compile-op  ,asdf-target))
       (hex:def-clos-dispatch ((self ,class) ,map-uri) ()
         (handle-asdf-output 'asdf:compile-op ,asdf-target 1)))))


(defmethod %handle-asdf-output (installation
                                op
                                component
                                output-files
                                output-num )
  (bt:with-lock-held (*lock*)
    (let ((output (elt output-files output-num)))
      (when (or
             #+lispworks
             (not (hcl:delivered-image-p))
             ;; in case we delete ~/.cache
             (not (path:-e output)))
        (asdf:operate op component))
      (assert (path:-e output))
      (handler-case
          (hunchentoot:handle-static-file output)
        #+lispworks
        (comm:socket-io-error (e)
          (values))))))
