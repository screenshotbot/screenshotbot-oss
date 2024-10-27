;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :util)

(defvar *delivered-image* nil)

(defun system-source-directory (system)
  (declare (optimize (speed 0) (debug 3)))
  ;; A version of asdf:system-source-directory that will also work in
  ;; delivered images.
  (cond
    (*delivered-image*
     (cond
       ((equal (string :web.all) (str:upcase system))
        #+lispworks
        (hcl:get-working-directory))
       (t
        (pathname (format nil "~a/" (str:downcase system))))))
    (t
     (asdf:system-source-directory system))))

(defun %asdf-relpath (path)
  (make-pathname
   :name (pathname-name path)
   :type (pathname-type path)
   :device nil
   :host nil
   :version nil
   :defaults
   (util/misc:relpath
    ;; The actual file may not exist at this point
    (truename (ensure-directories-exist (cl-fad:pathname-directory-pathname path)))
    (truename (path:catdir (asdf:system-source-directory :util) "../../")))))

(defun relative-system-source-directory (system)
  (%asdf-relpath
   (asdf:system-source-directory system)))

(defun relative-output-files (op component)
  (mapcar #'%asdf-relpath
          (asdf:output-files op (asdf:find-component component nil))))
