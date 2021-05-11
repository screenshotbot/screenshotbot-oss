;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :build-utils/wild-module
  (:use :cl
        :asdf
        :alexandria)
  (:export :*-module))
(in-package :build-utils/wild-module)

(defclass *-module (module)
  ((component-class :accessor wild-module-component-class
                    :initform 'static-file :initarg :component-class)
   (component-options :accessor wild-module-component-options
                      :initform nil :initarg :component-options)))

(defmethod (setf component-children) (new-value (module *-module))
  (when new-value
    (asdf::sysdef-error "Cannot explicitly set wild-module ~A's children components. Please ~
use a wild pathname instead." module)))

(defmethod reinitialize-instance :after ((self *-module) &key)
  (let ((pathname (component-pathname self)))
    (unless (and pathname (wild-pathname-p pathname))
      (asdf::sysdef-error "Wild-module ~A specified with non-wild pathname ~A."
                    self pathname))
    (setf (slot-value self 'components)
          (let* ((files (uiop:directory* pathname))
                 (class (wild-module-component-class self))
                 (options (wild-module-component-options self)))
            (mapcar (lambda (file)
                      (apply #'make-instance class
                             :name (namestring file)
                             :pathname file
                             :parent self
                             options))
                    files)))
    (asdf::compute-children-by-name self)
    (values)))

(defmethod input-files ((o compile-op) (c *-module)) ())
(defmethod input-files ((o load-op) (c *-module)) ())
