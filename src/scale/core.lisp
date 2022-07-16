(defpackage :scale/core
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:create-instance
   #:delete-instance
   #:wait-for-ready
   #:with-instance
   #:scp
   #:encode-bash-command
   #:add-url
   #:ssh-run))
(in-package :scale/core)

(defvar *last-instance*)
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
  (let ((instance (apply #'create-instance args))
        (delete? t))
   (unwind-protect
        (restart-case
            (progn
              (wait-for-ready instance)
              (funcall fn instance))
          (leave-instance-as-is ()
            (setf *last-instance* instance)
            (setf delete? nil)))
     (when delete?
      (delete-instance instance)))))

(defmacro with-instance ((instance &rest create-instance-args) &body body)
  `(call-with-instance
    (lambda (,instance)
      ,@body)
    ,@create-instance-args))

(auto-restart:with-auto-restart ()
 (defun download-file (url output)
   (log:info "Downloading ~a" url)
   (let ((input (util/request:http-request
                 url
                 :want-stream t
                 :force-binary t)))
     (with-open-file (output output :element-type '(unsigned-byte 8)
                                     :direction :output
                                     :if-exists :supersede)
       (uiop:copy-stream-to-stream
        input
        output
        :element-type '(unsigned-byte 8))))))


(defun add-url (instance url output)
  (let ((cache (ensure-directories-exist
                (make-pathname
                 :name (ironclad:byte-array-to-hex-string (md5:md5sum-string url))
                 :defaults "build/web-cache/"))))
    (unless (path:-e cache)
      (download-file url cache))
    (assert (path:-e cache))
    (scp instance cache output)))
