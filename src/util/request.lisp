(defpackage :util/request
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:http-request))
(in-package :util/request)

(defun http-request (url &rest args &key headers-as-hash-table
                                      want-string
                                      want-stream
                                      (verify #-(or mswindows win32) :required
                                              #+(or mswindows win32) nil)
                     &allow-other-keys)
  (let ((args (a:remove-from-plist args :headers-as-hash-table
                                   :want-string
                                   #+ccl :connection-timeout
                                   #-lispworks :read-timeout)))

    (when want-string
      (setf want-stream t))

    (multiple-value-bind (res status headers)
        (apply #'drakma:http-request url
                 :want-stream want-stream
                 :verify verify
                 args)
      (values
       (cond
         (want-string
          (uiop:slurp-input-stream 'string res))
         (t
          res))
       status
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
