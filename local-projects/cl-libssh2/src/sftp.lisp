;; -*- mode: lisp; tab-width: 4; ident-tabs-mode: nil -*-

(in-package :libssh2)

(defmacro with-sftp ((sftp-session ssh-connection) &body body)
  `(let* ((,sftp-session (libssh2-sftp-init (session ,ssh-connection)))
          (*sftp-session* ,sftp-session))
     (if (or (null ,sftp-session) (null-pointer-p ,sftp-session))
         (error "Cannot establish an SFTP session for SSH connection to ~A (port ~A)" (host,ssh-connection) (port ,ssh-connection))
         (unwind-protect
              (handler-bind ((libssh2-invalid-error-code
                              (lambda (condition)
                                (declare (ignore condition))
                                (throw-last-error (session ,ssh-connection)))))
                ,@body)
           (libssh2-sftp-shutdown ,sftp-session)))))


(defun ends-with? (name extension)
  (assert (> (length extension) 0))
  (assert (> (length name) 0))
  (let ((pos (search extension name :from-end t)))
    (when (and pos (= pos (- (length name) (length extension))))
      t)))

(defun ends-with-any? (name extensions)
  (dolist (ext extensions)
    (when (ends-with? name ext)
      (return t))))

(defun sftp-list-directory (ssh-connection path &key (maxfiles most-positive-fixnum) (extensions nil))
  "Return a list of files in directory `PATH' on the server to which we are connected with `SSH-CONNECTION'.
Restrict the number of files to retrieve by providing
  - `MAXFILES' - a fixnum (which is by default set to an insanely large number)
  - `EXTENSIONS' - a list of strings (default: nil)

It is possible to combine `MAXFILES' and `EXTENSIONS' (retrieve 5 files with extensions '(\".json\", \".js\")) "

  (with-sftp (sftp ssh-connection)
    (let* ((handle (libssh2-sftp-open-ex sftp path 0 0 (foreign-enum-value 'sftp-open-types :dir)))
           (buffer (foreign-alloc :char :count 1024 :initial-element 0))
           (longentry (foreign-alloc :char :count 1024 :initial-element 0)))
      (unwind-protect
           (with-foreign-object (attrs '(:struct _libssh2-sftp-attributes))
             (loop while (> (libssh2-sftp-readdir-ex handle buffer 1024 longentry 1024 attrs) 0)
                   while (> maxfiles (length files))
                   for attr-plist = (convert-from-foreign attrs '(:struct _libssh2-sftp-attributes))
                   do (ssh2.dribble "Attributes of ~A: ~A, permissions: ~A" (foreign-string-to-lisp buffer) attr-plist (foreign-bitfield-symbols 'sftp-modes (getf attr-plist 'permissions)))
                   when (or (null extensions)
                            (ends-with-any? (foreign-string-to-lisp buffer) extensions)) collect (foreign-string-to-lisp buffer) into files
                   finally (return files)))
        (when handle (libssh2-sftp-close-handle handle))
        (when buffer (foreign-free buffer))
        (when longentry (foreign-free longentry))))))


(defconstant +sftp-read-buffer-size+ 1000000)

;; TODO: refactor some of the commonalities into a macro?
(defun sftp-get (ssh-connection remote-path local-path)
  "Receive a remote file `PATH' on the server to which we are connected with `SSH-CONNECTION' to a local file at `LOCAL-PATH'."
  (with-sftp (sftp ssh-connection)
    (let ((handle)
          (buffer))
      (unwind-protect
        (progn
          (ssh2.debug "Trying to retrieve remote file ~A to local file ~A" remote-path local-path))
          (setf handle (libssh2-sftp-open-ex sftp remote-path (foreign-bitfield-value 'sftp-flags '(:read)) 0 :file))
          (setf buffer (foreign-alloc :char :count +sftp-read-buffer-size+ :initial-element 0))
          (with-open-file (out local-path :direction :output :if-exists :supersede :element-type '(signed-byte 8))
            (loop for numbytes = (libssh2-sftp-read handle buffer +sftp-read-buffer-size+)
                  do (ssh2.dribble "libssh2-sftp-read returned numbytes=~A" numbytes)
                  while (> numbytes 0)
                  do
                  (write-sequence (cffi:convert-from-foreign buffer `(:array :char ,numbytes)) out)))
          (ssh2.debug "Remote file ~A was written to ~A" remote-path local-path))
        (when handle (libssh2-sftp-close-handle handle))
        (when buffer (foreign-free buffer)))))

(defun sftp-delete (ssh-connection remote-path)
  "Delete a remote file `PATH' on the server to which we are connected with `SSH-CONNECTION'."
  (with-sftp (sftp ssh-connection)
    (ssh2.debug "Trying to delete remote file ~A" remote-path)
    (let ((result (libssh2-sftp-unlink-ex sftp remote-path)))
      (ssh2.debug "Deleting ~A resulted in ~A." remote-path result))))
