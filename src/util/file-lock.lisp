;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/file-lock
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:util-store-file-lock
   #:release-file-lock
   #:file-lock))
(in-package :util/file-lock)

(defvar *loaded* nil)

(defun register-native (&key force)
  (when force
    (setf *loaded* nil))
  (unless *loaded*
    (let ((output (asdf:output-file 'asdf:compile-op
                                    (asdf:find-component :util/store "store-native"))))

      #-lispworks
      (cffi:load-foreign-library
       output)
      #+lispworks
      (progn
        (when force
          (fli:disconnect-module :store-native))
        (fli:register-module
         :store-native
         :real-name output))
      (setf *loaded* t))))

(defclass file-lock ()
  ((file :initarg :file)
   (fd :initform nil)
   (shared :initform nil
           :initarg :shared)
   (timeout :initarg :timeout
            :initform 600)))

(cffi:defcfun ("util_store_file_lock" %util-store-file-lock) :int
  (filename :string))

(cffi:defcfun ("util_store_file_lock_shared" %util-store-file-lock-shared) :int
  (filename :string))

(cffi:defcfun "util_store_file_unlock" :int
  (fd :int))

;; just in case we want to interrupt the lock process
(defun util-store-file-lock (file &key shared)
  (cond
    (shared
     (%util-store-file-lock-shared file))
    (t
     (%util-store-file-lock file))))

(define-condition lock-not-held (error)
  ()
  (:report "Attempting to unlock a lock that is not being held"))

(define-condition could-not-get-lock (error)
  ()
  (:report "Could not get lock"))

(defmethod initialize-instance :after ((file-lock file-lock) &key file)
  (register-native)
  (let ((start-time (get-universal-time)))
   (with-slots (fd shared timeout) file-lock
     (log:info "Waiting for file lock: ~a" file)

     (loop for res = (util-store-file-lock (uiop:native-namestring file)
                                           :shared shared)
           while (< res 0)
           do (progn
                (when (> (get-universal-time)
                         (+ start-time timeout))
                  (error 'could-not-get-lock))
                (log:info "Could not get file lock ~a, will try again in 5 seconds" file)
                (sleep 5))
           finally
              (progn
                (setf fd res)
                (log:info "Got file lock"))))))

(defmethod release-file-lock ((file-lock file-lock))
  (with-slots (fd) file-lock
    (unless fd
      (error 'lock-not-held))
    (unless (eql 0
                 (util-store-file-unlock fd))
      (warn "Could not unlock file lock"))
    (setf fd nil)))
