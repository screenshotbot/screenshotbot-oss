;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/native-module
  (:use #:cl)
  (:export
   #:make-native-module
   #:load-module)
  (:local-nicknames #-lispworks (#:fli #:util/fake-fli)))
(in-package :util/native-module)

(defclass native-module ()
  ((name :initarg :name
         :reader name)
   (pathname-provider :initarg :pathname-provider
                      :reader pathname-provider)
   (embedded-p :initform nil
               :accessor embedded-p)
   (loaded-p :initform nil
             :accessor loaded-p)
   (verify :initarg :verify
           :accessor verify)))

(defun make-native-module (name system component-name &key (verify (lambda ())))
  (make-instance 'native-module
                 :name name
                 :verify verify
                 :pathname-provider (lambda ()
                                      (asdf:output-file
                                       'asdf:compile-op
                                       (asdf:find-component system
                                                            (list component-name))))))

(defmethod module-pathname ((self native-module))
  (funcall (pathname-provider self)))

(defmethod embed-module ((self native-module))
  #+lispworks
  (progn
    (fli:get-embedded-module (name self)
                             (module-pathname self))
    (setf (embedded-p self) t)))

(defmethod load-module ((self native-module) &key force)
  #+linux
  (when force
    #+lispworks
    (fli:disconnect-module (name self))
    (setf (loaded-p self) nil))
  (util:or-setf
   (loaded-p self)
   (progn
     (cond
       ((embedded-p self)
        #+lispworks
        (fli:install-embedded-module (name self)))
       (t
        (fli:register-module
         (name self)
         :real-name (module-pathname self))))

     (funcall (verify self))
     t)
   :thread-safe t))
