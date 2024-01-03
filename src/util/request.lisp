(defpackage :util/request
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:common-lisp
                #:with-open-stream)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:http-request
   #:http-success-response?
   #:engine))
(in-package :util/request)

(defun http-success-response? (response-code)
  (<= 200 response-code 204))

(defun fix-bad-chars (url)
  (cond
    ((stringp url)
     (str:replace-all "|" "%7C" url))
    (t  url)))


(def-easy-macro wrap-ssl-errors (&key ensure-success want-stream &fn fn)
  "When calling http-request, most code expects it not crash if the
 network request fails. However if there's an SSL error it will lead
 to a crash.

   If ensure-success is not set, we'll wrap errors to make them look
 like a 502 error."
  (cond
    (ensure-success
     ;; We don't need to handle SSL errors
     (funcall fn))
    (t
     (flet ((handle-error (e)
              (declare (ignore e))
              (values
               (cond
                 (want-stream
                  (make-string-input-stream ""))
                 (t
                  ""))
               502 nil)))
      (handler-case
          (funcall fn)
        #+lispworks
        (comm:ssl-verification-failure (e)
          (handle-error e))
        #+lispworks
        (comm:ssl-failure (e)
          (handle-error e))
        #+lispworks
        (comm:socket-create-error (e)
          (handle-error e)))))))


(defclass force-ip-address-engine ()
  ())

(defclass core-engine ()
  ())

(defclass engine (core-engine)
  ())


(defvar *engine* (make-instance 'engine)
  "Default request engine. Does not cache, does not use cookies, does not
use stream pools.")

(defmethod http-request-impl ((engine force-ip-address-engine) url
                              &rest args
                              &key force-address (force-port 80)
                                (connection-timeout 20)
                                (read-timeout 20)
                                (write-timeout 20)
                              &allow-other-keys)
  (cond
    (force-address
     (let ((stream (flex:make-flexi-stream
                    (chunga:make-chunked-stream
                     #+lispworks
                     (comm:open-tcp-stream
                      force-address force-port
                      :element-type 'flex:octet
                      :timeout connection-timeout
                      :read-timeout read-timeout
                      :write-timeout write-timeout
                      :errorp t)
                     #-lispworks
                     (usocket:socket-stream
                      (usocket:socket-connect host port
                                              :element-type 'octet
                                              :timeout connection-timeout
                                              :nodelay :if-supported)))
                    :external-format drakma::+latin-1+)))
       (with-open-stream (stream stream)
         (apply #'call-next-method
                engine url
                :stream stream
                (alexandria:remove-from-plist args :force-address :force-port)))))
    (t
     (call-next-method))))

(defmethod http-request-impl ((engine engine) url &key &allow-other-keys)
  (call-next-method))

(defmethod http-request-impl ((engine core-engine)
                              url &rest args &key headers-as-hash-table
                                               want-string
                                               ensure-success
                                               additional-headers
                                               accept-gzip
                                               want-stream
                                               (verify #-(or mswindows win32) :required
                                                       #+(or mswindows win32) nil)
                              &allow-other-keys)
  (let* ((url (fix-bad-chars url))
         (args (a:remove-from-plist args :headers-as-hash-table
                                    :accept-gzip
                                    :ensure-success
                                    :want-string
                                    #+ccl :connection-timeout
                                    #-lispworks :read-timeout)))

    (when accept-gzip
      (push
       (cons :accept-encoding "gzip")
       additional-headers))
    (when want-string
      (setf want-stream t))

    (multiple-value-bind (res status headers)
        (wrap-ssl-errors (:ensure-success ensure-success :want-stream want-stream)
          (apply #'drakma:http-request url
                 :want-stream want-stream
                 :additional-headers additional-headers
                 :verify verify
                 args))
      (flet ((maybe-ensure-success (&optional response)
               (declare (ignore response))
               (when (and ensure-success
                          (not (http-success-response? status)))
                 (error "Got response code ~a when downloading ~a" status url))))
        (handler-bind ((error (lambda (e)
                                (declare (ignore e))
                                ;; We're not going to actually return the stream
                                (when (streamp res)
                                  (close res)))))

          (values
           (cond
             (want-string
              (let ((response (uiop:slurp-input-stream 'string res)))
                ;; by calling this here, we'll have the response in the
                ;; stack trace.
                (maybe-ensure-success response)
                response))
             (t
              (maybe-ensure-success)
              res))
           status
           (cond
             (headers-as-hash-table
              (make-header-hash-table headers))
             (t
              headers))))))))

(defun http-request (url &rest args &key (engine *engine*) &allow-other-keys)
  (apply #'http-request-impl
         engine
         url
         (a:remove-from-plist args :engine)))

(defun make-header-hash-table (headers)
  (let ((ret (make-hash-table :test #'equal)))
    (loop for (key . value) in headers
          do (setf (gethash (string-downcase key) ret) value))
    ret))
