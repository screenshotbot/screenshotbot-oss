(defpackage :util/logrotate
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/logrotate)

(defun logrotate (file
                  &key (max-size 10 #| max size in MB |#)
                    (rotate 5))
  (let ((state-file (pathname "~/.logrotate.status")))

    (uiop:with-temporary-file (:stream config
                               :pathname config-pathname)
      (format config
              "~a {
   rotate ~a
   weekly
   size ~ak
   nocompress
}
"
             (namestring file)
             rotate
             (* max-size 1024))
     (close config)
     (multiple-value-bind (out err ret)
         (uiop:run-program (list "/sbin/logrotate"
                                 "--state" (namestring state-file)
                                 (namestring config-pathname))
                           :output t
                           :error-output 'string
                           :ignore-error-status t)
       (declare (ignore out))
       (unless (= ret 0)
         (error "logrotate error: ~a" err))))))
