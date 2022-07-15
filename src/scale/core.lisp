(defpackage :scale/core
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:create-instance
   #:delete-instance
   #:wait-for-ready
   #:with-instance
   #:scp
   #:encode-bash-command))
(in-package :scale/core)

(defgeneric create-instance (provider type &key region))

(defgeneric delete-instance (instance))

(defgeneric wait-for-ready (instance))

(defgeneric ssh-run (instance cmd &key output error-output))

(defmethod scp (instance local-file remote-file))

(defmethod ssh-run (instance (cmd list) &rest args &key &allow-other-keys)
  (apply #'ssh-run
           instance
           (encode-bash-command cmd)
           args))


(defun encode-bash-command (list)
  (str:join " " (mapcar #'uiop:escape-sh-token list)))

(Defun call-with-instance (fn &rest args)
  (let ((instance (apply #'create-instance args)))
    (unwind-protect
         (progn
           (wait-for-ready instance)
           (funcall fn instance))
      (delete-instance instance))))

(defmacro with-instance ((instance &rest create-instance-args) &body body)
  `(call-with-instance
    (lambda (,instance)
      ,@body)
    ,@create-instance-args))
