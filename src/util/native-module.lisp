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
   #:make-system-module
   #:*lib-cache-dir*)
  (:local-nicknames #-lispworks (#:fli #:util/fake-fli)))
(in-package :util/native-module)

(defvar *lib-cache-dir* nil)

(defclass native-module ()
  ((name :initarg :name
         :reader name)
   (lock :initform (bt:make-lock)
         :reader lock)
   (pathname-provider :initarg :pathname-provider
                      :accessor pathname-provider)
   (pathname-flag :initarg :pathname-flag
                  :reader pathname-flag
                  :initform :real-name)
   (embedded-p :initform nil
               :accessor embedded-p)
   (loaded-p :initform nil
             :accessor loaded-p)
   (verify :initarg :verify
           :initform (lambda ())
           :accessor verify)
   (module-data :initform nil
                :accessor module-data
                :documentation "The actual binary data of the module, when installing it manually.")))

(defmethod print-object ((self native-module) out)
  (format out "#<NATIVE-MODULE ~a>" (name self)))

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

(defun ensure-pathname-type (&key type defaults)
  (cond
   ((equal "so" (pathname-type defaults))
    (make-pathname :type type :defaults defaults))
   (t
    defaults)))

(defun find-module (pathname)
  "During the embed step, we need the absolute pathname"
  (let ((path (list
               "/usr/lib/"
               "/usr/local/lib/"
               "/usr/lib/x86_64-linux-gnu/"
               "/opt/homebrew/lib/")))

     (loop for p in path
           for abs =
             (ensure-pathname-type
              :type #+darwin "dylib"
              #+linux "so"
              #+mswindows "dll"
              :defaults
              (path:catfile p pathname))
           if (path:-e abs)
             return abs)))

(defun read-file-content (pathname)
  (with-open-file (stream pathname
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((res (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence res stream)
      res)))

(defmethod embed-module ((self native-module))
  (setf (pathname-provider self)
        (let ((pathname (funcall (pathname-provider self))))
          (lambda () pathname)))
  (setf (module-data self)
        (read-file-content
         (find-module (module-pathname self))))
  (setf (embedded-p self) t))

(defmethod load-embedded-module ((self native-module))
  (let ((output (path:catfile *lib-cache-dir* (make-pathname
                                               :directory `(:relative)
                                               :defaults (module-pathname self)))))
    (delete-file output)
    (log:debug "Loading module ~a, ~a" (name self) output)
    (with-open-file (stream output
                            :direction :output
                            :element-type '(unsigned-byte 8))
      (write-sequence (module-data self)
                      stream))
    (fli:register-module (name self)
                         :real-name output
                         :connection-style :immediate)))

(defmethod load-module :around (self &key force)
  (handler-bind ((error (lambda (e)
                          (declare (ignore e))
                          ;; If a module does fail, it's probably very
                          ;; early on, so we want to print some
                          ;; additional debugging information.
                          (log:info "Error loading module: ~a" self))))
    (call-next-method)))

(defmethod load-module ((self native-module) &key force)
  #+linux
  (when force
    #+lispworks
    (fli:disconnect-module (name self))
    (setf (loaded-p self) nil))
  (let ((needs-verify nil))
   (util:or-setf
    (loaded-p self)
    (progn
      (cond
        ((embedded-p self)
         (load-embedded-module self))
        (t
         (fli:register-module
          (name self)
          (pathname-flag self) (module-pathname self))))
      (setf needs-verify t)
      t)
    :thread-safe t
    :lock (lock self))
    (when needs-verify
      (funcall (verify self)))
    t))
