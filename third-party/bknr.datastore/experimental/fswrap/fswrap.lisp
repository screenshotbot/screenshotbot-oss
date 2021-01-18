
(in-package :bknr.web)

(use-package :acl-compat.socket)

(defun fswrap-server (&key (port 2931))
  (let ((listener-socket (make-socket :local-port port :connect :passive)))
    (unwind-protect
	 (loop for client = (acl-compat.socket:accept-connection listener-socket)
	       do (format t "new client~%")
	       do (loop for request = (read-line client nil nil)
			while request
			do (format t "got request ~a~%" request)
			do (destructuring-bind
				 (command arg)
			       (split " " request :limit 2)
			     (cond
			       ((equal command "translate")
				(format t "translate ~a~%" arg)
				(format client "~a~%" arg)
				(finish-output client))
			       (t
				(format t "unknown command~%"))))
			finally (close client)))
      (close listener-socket))))