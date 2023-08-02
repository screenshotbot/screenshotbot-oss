(defpackage :util/file-lock
  (:nicknames :file-lock)
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:export
   #:release-file-lock
   #:file-lock
   #:make-file-lock
   #:with-file-lock))
(in-package :file-lock)

(defclass file-lock ()
  ((file :initarg :file
         :reader filename)
   (stream :initform nil
           :accessor file-lock-stream)))

(defconstant lock-sh 1 "Shared lock.")
(defconstant lock-ex 2 "Exclusive lock.")
(defconstant lock-un 8 "Unlock.")
(defconstant lock-nb 4 "Don't block when locking.")

;; In theory, this is identical to the cffi:defcfun in the next step.
;; But, our code is a bit reliant on this and we don't like dealing
;; making too many FLI changes on our production server. We know the
;; FLI version works for us, so we're leaving it as is. (PS. If you
;; the reader are trying to add Windows support, feel free to use
;; CFFI.)
#+lispworks
(fli:define-foreign-function (flock "flock")
    ((fd :int)
     (operation :int))
  :result-type :int)

#-lispworks
(cffi:defcfun ("flock" flock) :int
  (fd :int)
  (operation :int))

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
  (let ((stream (file-lock-stream self)))
    #+lispworks
    (let ((handle (slot-value stream 'stream::file-handle)))
      handle)
    #+sbcl
    (sb-posix:file-descriptor stream)
    #- (or sbcl lispworks)
    (error "Can't get file-handle on this platform, only SBCL and Lispworks supported. Please send a Pull Request, it shouldn't be too hard:)")))

(defun get-unix-error ()
  (or
   #+(and lispworks linux)
   (lw:get-unix-error (lw:errno-value))))

(defmethod ensure-stream ((self file-lock))
  (unless (file-lock-stream self)
    (setf (file-lock-stream self)
          (open (ensure-directories-exist
                 (filename self))
                :if-does-not-exist :create
                :if-exists :append
                :direction :output))))

(defmethod acquire-file-lock ((self file-lock) &key (sharedp nil)
                                                 (timeout 600))
  (ensure-stream self)
  (log:info "Waiting for file lock: ~a (shared: ~a, timeout: ~a)" (filename self)
            sharedp timeout)
  (let ((start-time (get-universal-time)))
    (loop for res = (flock
                     (file-handle self)
                     (logior
                      (if sharedp lock-sh lock-ex)
                      lock-nb))
          for ctr from 0
          while (< res 0)
          do (progn
               (when (> (get-universal-time)
                        (+ start-time timeout))
                 (log:info "Could not get file lock: ~a" (get-unix-error))
                 (error 'could-not-get-lock))
               (when (= 0 (mod ctr 25))
                 (log:info "Could not get file lock ~a, will try again on ~a: ~a"
                           (get-unix-error)
                           (bt:current-thread)
                           (filename self)))
               (sleep 0.2))
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

(defun make-file-lock (&rest args &key (file (error "must provide filename"))
                       &allow-other-keys)
  (declare (ignorable file))
  (cond
    #+(or (not (:or :sbcl :lispworks)) windows)
    (t
     (warn "Lock files not supported on this platform, and the lock will be a no-op.")
     (make-instance 'noop-file-lock :file file))
    (t
     (apply #'make-instance 'file-lock args))))

(def-easy-macro with-file-lock (&key file &binding lock
                                     &fn fn)
  (let ((lock (make-file-lock :file file)))
    (unwind-protect
         (fn lock)
      (release-file-lock lock))))
