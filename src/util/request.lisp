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
   #:engine
   #:proxy-engine))
(in-package :util/request)

(defun http-success-response? (response-code)
  (<= 200 response-code 204))

(defun fix-bad-chars (url)
  (cond
    ((stringp url)
     (str:replace-all "|" "%7C" url))
    (t  url)))

;; NOT BEING USED
(define-condition request-error (error)
  ())

;; NOT BEING USED
(define-condition network-error (request-error)
  ()
  (:documentation "Errors that typically correspond to network failures, as opposed to
server failures."))

;; NOT BEING USED
(define-condition timeout-error (network-error)
  ())


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
              (log:warn "Got error: ~a" e)
              (values
               (cond
                 (want-stream
                  (make-string-input-stream ""))
                 (t
                  ""))
               502 nil)))
       (macrolet ((handle-errors (errors &body body)
                    `(handler-case
                         (progn ,@body)
                       ,@ (loop for error in errors
                                collect
                                `(,error (e)
                                         (handle-error e))))))
         (handle-errors #+lispworks (comm:ssl-verification-failure
                                     comm:ssl-failure
                                     comm:socket-create-error
                                     comm:ssl-closed)
                        #-lispworks ()
                        (funcall fn)))))))

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

(defmethod low-level-request ((engine core-engine)
                              url
                              &rest args
                                &key &allow-other-keys)
  (apply #'drakma:http-request url
         args))

(define-condition bad-response-code (error)
  ((code :initarg :code)
   (url :initarg :url
        :initform nil)
   (body :initarg :body
         :reader bad-response-code-body))
  (:report (lambda (e output)
             (with-slots (code url) e
               (format output "Got response code ~a when downloading ~a" code url)))))


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
  "WANT-STRING just means we will never return a binary array, it will
always be converted to a UTF-8 string even if the encoding format is
not specified."
  (let* ((old-body-format-function drakma:*body-format-function*)
         (drakma:*body-format-function* old-body-format-function))
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
        (setf drakma:*body-format-function*
              (lambda (headers external-format-in)
                (or
                 (funcall old-body-format-function headers external-format-in)
                 :utf-8))))
      
      (multiple-value-bind (res status headers)
          (wrap-ssl-errors (:ensure-success ensure-success :want-stream want-stream)
            (let ((drakma:*text-content-types*
                    (list*
                     (cons "application" "json")
                     drakma:*text-content-types*)))
              (apply #'low-level-request engine
                     url
                     :want-stream want-stream
                     :additional-headers additional-headers
                     :verify verify
                     args)))
        (flet ((maybe-ensure-success (&optional response)
                 (declare (ignore response))
                 (when (and ensure-success
                            (not (http-success-response? status)))
                   (log:info "headers: ~a" headers)
                   (error 'bad-response-code
                          :code status
                          :url url
                          :body (when want-string
                                  ;; otherwise, res is a stream and we can't do much with it
                                  res)))))
          (handler-bind ((error (lambda (e)
                                  (declare (ignore e))
                                  ;; We're not going to actually return the stream
                                  (when (streamp res)
                                    (close res)))))
            (maybe-ensure-success res)
            (values
             res
             status
             (cond
               (headers-as-hash-table
                (make-header-hash-table headers))
               (t
                headers)))))))))

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

(defclass proxy-engine ()
  ((proxy :initarg :proxy
          :initform nil
          :reader proxy-url
          :documentation "A list with two values, an IP address and a port, which designates the HTTP proxy, or NIL if no proxy should be used")))

(defmethod http-request-impl ((self proxy-engine) url &rest args)
  (cond
    ((proxy-url self)
     (apply #'call-next-method
            self
            url
            :proxy (proxy-url self)
            args))
    (t
     (call-next-method))))
