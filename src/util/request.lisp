(defpackage :util/request
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:http-request))
(in-package :util/request)

(defun http-request (url &rest args &key headers-as-hash-table
                                      (verify #-(or mswindows win32) :required
                                              #+(or mswindows win32) nil)
                     &allow-other-keys)
  (let ((args (a:remove-from-plist args :headers-as-hash-table
                                   #+ccl :connection-timeout
                                   #-lispworks :read-timeout)))

    (multiple-value-bind (res status headers)
        (apply #'drakma:http-request url
                 :verify verify
                 args)
      (values
       res status
       (cond
         (headers-as-hash-table
          (make-header-hash-table headers))
         (t
          headers))))))

(defun make-header-hash-table (headers)
  (let ((ret (make-hash-table :test #'equal)))
    (loop for (key . value) in headers
          do (setf (gethash (string-downcase key) ret) value))
    ret))
