(defpackage :util/request
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:http-request
   #:http-success-response?))
(in-package :util/request)

(defun http-success-response? (response-code)
  (<= 200 response-code 204))

(defun fix-bad-chars (url)
  (cond
    ((stringp url)
     (str:replace-all "|" "%7C" url))
    (t  url)))

(defun http-request (url &rest args &key headers-as-hash-table
                                      want-string
                                      ensure-success
                                      want-stream
                                      (verify #-(or mswindows win32) :required
                                              #+(or mswindows win32) nil)
                     &allow-other-keys)
  (let* ((url (fix-bad-chars url))
         (args (a:remove-from-plist args :headers-as-hash-table
                                       :ensure-success
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
      (handler-bind ((error (lambda (e)
                              ;; We're not going to actually return the stream
                              (when (streamp res)
                                (close res)))))

        (when (and ensure-success
                   (not (http-success-response? status)))
          (error "Got response code ~a when downloading ~a" status url))
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
            headers)))))))

(defun make-header-hash-table (headers)
  (let ((ret (make-hash-table :test #'equal)))
    (loop for (key . value) in headers
          do (setf (gethash (string-downcase key) ret) value))
    ret))
