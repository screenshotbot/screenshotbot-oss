(in-package :util)

(let ((output (asdf:output-file 'asdf:compile-op
                                 (asdf:find-component :util "store-native"))))
  #-lispworks
  (cffi:load-foreign-library
   output)
  #+lispworks
  (fli:register-module
   :store-native
   :real-name output))


(defclass file-lock ()
  ((file :initarg :file)
   (fd :initform -1)))

(cffi:defcfun ("util_store_file_lock" %util-store-file-lock) :int
  (filename :string))

(cffi:defcfun "util_store_file_unlock" :int
  (fd :int))

;; just in case we want to interrupt the lock process
(defun util-store-file-lock (fd)
  (%util-store-file-lock fd))

(defmethod initialize-instance :after ((file-lock file-lock) &key file)
  (with-slots (fd) file-lock
    (log:info "Waiting for file lock: ~a" file)

    (loop for res = (util-store-file-lock (uiop:native-namestring file))
          while (< res 0)
          do (progn
               (log:info "Could not get file lock ~a, will try again in 5 seconds" file)
               (sleep 5))
          finally
             (progn
               (setf fd res)
               (log:info "Got file lock")))))

(defmethod release-file-lock ((file-lock file-lock))
  (with-slots (fd) file-lock
   (util-store-file-unlock fd)))
