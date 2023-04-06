;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/native-module
  (:use #:cl)
  (:import-from #:alexandria
                #:remove-from-plist)
  (:export
   #:make-native-module
   #:load-module
   #:make-system-module)
  (:local-nicknames #-lispworks (#:fli #:util/fake-fli)))
(in-package :util/native-module)

(defclass native-module ()
  ((name :initarg :name
         :reader name)
   (pathname-provider :initarg :pathname-provider
                      :reader pathname-provider)
   (pathname-flag :initarg :pathname-flag
                  :reader pathname-flag
                  :initform :real-name)
   (embedded-p :initform nil
               :accessor embedded-p)
   (loaded-p :initform nil
             :accessor loaded-p)
   (verify :initarg :verify
           :initform (lambda ())
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

(defun make-system-module (name &rest args &key file-name &allow-other-keys)
  (cond
    (file-name
     (make-instance 'native-module
                    :name name
                    :pathname-provider (lambda () file-name)
                    :pathname-flag :file-name))
    (t
     (apply #'make-instance 'native-module
            :name name
            (remove-from-plist args :file-name)))))

(defmethod module-pathname ((self native-module))
  (funcall (pathname-provider self)))

(defun find-module (pathname)
  "During the embed step, we need the absolute pathname"
  (let ((path (list
               "/usr/lib/"
               "/usr/local/lib/"
               "/usr/lib/x86_64-linux-gnu/")))
    (loop for p in path
          for abs = (path:catfile p pathname)
          if (path:-e abs)
            return abs)))

(defmethod embed-module ((self native-module))
  #+lispworks
  (progn
    (fli:get-embedded-module (name self)
                             (find-module (module-pathname self)))
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
         (pathname-flag self) (module-pathname self))))

     (funcall (verify self))
     t)
   :thread-safe t))
