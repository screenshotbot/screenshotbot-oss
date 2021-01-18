(in-package :bknr.utils)

(defvar +smb-wrapper-program+ "/usr/local/bin/smbpasswd-wrapper")

(define-condition smb-password-error ()
  ((message :initarg :message :accessor smb-password-error-message)))

(defmethod print-object ((error smb-password-error) stream)
  (format stream "#<~a ~a>"
	  (class-name (class-of error))
	  (smb-password-error-message error))
  error)

(defun set-smb-password (username password &key (create t))
  (unless (and username password)
    (error (make-condition 'smb-password-error :message "please specify both username and password")))
  (let ((args (list username password)))
    (when create
      (push "-a" args))
    (push "smbpasswd" args)
    (with-output-to-string (stream)
      #+allegro
      (excl:run-shell-command (apply #'concatenate
				'string
				+smb-wrapper-program+
				args)
			 :output stream :error-output stream)
      #+cmu
      (let ((process
	     (ext:run-program +smb-wrapper-program+ args :output stream :error :output)))
	(unwind-protect
	     (unless (zerop (ext:process-exit-code process))
	       (error (make-condition 'smb-password-error :message (get-output-stream-string stream))))
	  (ext:process-close process)))
      #+openmcl
      (ccl::run-program +smb-wrapper-program+
			args
			:output stream)
      #+sbcl
      (let ((process
	     (sb-ext:run-program +smb-wrapper-program+ args :output stream :error :output)))
	(unwind-protect
	     (unless (zerop (process-exit-code process))
	       (error (make-condition 'smb-password-error :message (get-output-stream-string stream))))
	  (process-close process))))))
