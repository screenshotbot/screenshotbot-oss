;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/file-lock
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    #-lispworks
                    (#:fli #:util/fake-fli)                    )
  (:export
   #:release-file-lock
   #:file-lock
   #:make-file-lock))
(in-package :util/file-lock)

(defclass file-lock ()
  ((file :initarg :file
         :reader filename)
   (stream :initform nil
           :accessor file-lock-stream)))

(defconstant lock-sh 1 "Shared lock.")
(defconstant lock-ex 2 "Exclusive lock.")
(defconstant lock-un 8 "Unlock.")
(defconstant lock-nb 4 "Don't block when locking.")

(fli:define-foreign-function (flock "flock")
    ((fd :int)
     (operation :int))
  :result-type :int)

(define-condition lock-not-held (error)
  ()
  (:report "Attempting to unlock a lock that is not being held"))

(define-condition could-not-get-lock (error)
  ()
  (:report "Could not get lock"))

(defmethod initialize-instance :after ((self file-lock) &key file
                                                          (sharedp nil)
                                                          (acquire t)
                                                          (timeout 600))
  (when acquire
    (acquire-file-lock self
                       :timeout timeout
                       :sharedp sharedp)))

(defmethod file-handle ((self file-lock))
  #+lispworks
  (slot-value (file-lock-stream self) 'stream::file-handle))

(defun get-unix-error ()
  (or
   #+(and lispworks linux)
   (lw:get-unix-error (lw:errno-value))))

(defmethod ensure-stream ((self file-lock))
  (unless (file-lock-stream self)
    (setf (file-lock-stream self)
          (open (filename self) :if-does-not-exist :create
                                :if-exists :append))))

(defmethod acquire-file-lock ((self file-lock) &key (sharedp nil)
                                                 (timeout 600))
  (ensure-stream self)
  (log:info "Waiting for file lock: ~a" (filename self))
  (let ((start-time (get-universal-time)))
    (loop for res = (flock
                     (file-handle self)
                     (logior
                      (if sharedp lock-sh lock-ex)
                      lock-nb))
         while (< res 0)
         do (progn
              (when (> (get-universal-time)
                       (+ start-time timeout))
                (error 'could-not-get-lock))
              (log:info "Could not get file lock ~a, will try again: ~a"
                        (get-unix-error)
                        (filename self))
              (sleep 5))
         finally
            (progn
              (log:info "Got file lock")))))

(defmethod release-file-lock ((self file-lock))
  (unless (file-lock-stream self)
    (error 'lock-not-held))
  (let ((res (flock
              (file-handle self)
              (logior
               lock-un lock-nb))))
    (when (< res 0)
      (error "Could not unlock: ~a"
             (get-unix-error)))

    (close (file-lock-stream self))
    (setf (file-lock-stream self) nil)))

(defclass noop-file-lock ()
  ((file :initarg :file)))

(defmethod acquire-file-lock ((self noop-file-lock) &key &allow-other-keys)
  (values))

(defmethod release-file-lock ((self noop-file-lock))
  (values))

(defun make-file-lock (&key (file (error "must provide filename")))
  (cond
    #+(or (not :lispworks) windows)
    (t
     (make-instance 'noop-file-lock :file file))
    (t
     (make-instance 'file-lock :file file))))
