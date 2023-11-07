;;;; Copyright 2019 Modern Interpreters Inc.

(in-package #:hunchentoot-multi-acceptor)

(defclass multi-request (request)
  ())

(defclass multi-acceptor (acceptor)
  ((sub-acceptors :initform nil :accessor sub-acceptors)
   (listen-fd :initarg :listen-fd
              :initform nil
              :accessor listen-fd)))

(defmethod initialize-instance :after ((acceptor multi-acceptor) &key &allow-other-keys)
  (setf (acceptor-request-class acceptor) 'multi-request))

(defvar *default-acceptor*
  (make-instance 'hunchentoot:easy-acceptor
                 :port 1
                 :name 'default-acceptor))

(hunchentoot:define-easy-handler (default-route :uri "/" :acceptor-names (list 'default-acceptor)) ()
  (format nil "Default Acceptor (for: ~a)" (hunchentoot:host)))

;; (define-easy-handler (s-default-message :uri "/foo" :acceptor-names '(default-acceptor)) ()
;;   (format nil "error, hostname not set up for: ~a" (host *request*)))

(defun copy-request (request acceptor)
  (let ((*acceptor* acceptor))
    (make-instance (acceptor-request-class acceptor)
                   :acceptor acceptor
                   :local-addr (local-addr request)
                   :local-port (local-port request)
                   :remote-addr (remote-addr request)
                   :remote-port (remote-port request)
                   :headers-in (headers-in request)
                   :content-stream (hunchentoot::content-stream request)
                   :method (request-method request)
                   :uri (request-uri request)
                   :server-protocol (server-protocol request))))

#+nil (copy-request
 (let ((*acceptor* (make-instance 'acceptor))
       (*reply* (make-instance 'reply)))
   (make-instance 'request
                  :local-addr "343"
                  :local-port 20
                  :method nil
                  :uri nil
                  :server-protocol nil
                  :remote-addr "dfdf"
                  :remote-port 30
                  :headers-in nil
                  :content-stream nil)) (make-instance 'acceptor))

(defmethod process-request ((request multi-request))
   (flet ((dispatch-acceptor (acceptor)
            (return-from process-request
              (let* ((*acceptor* acceptor)
                     (*request* (copy-request request *acceptor*)))
                (process-request *request*)))))

   (let ((acceptor (request-acceptor request)))
     (let ((host (car (str:split ":" (host request)))))
       (loop for sub in (sub-acceptors acceptor)
             if (equal (car sub) host)
               do (dispatch-acceptor (cdr sub)))

       (dispatch-acceptor *default-acceptor*)))))

(defun listen-on-fd (fd &key element-type)
  #+sbcl(let ((sock (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :stream
                             :protocol :tcp
                             :descriptor fd)))
          (usocket::make-stream-server-socket sock :element-type element-type))
  #-sbcl (error "Can't listen on file descriptor; this feature is only supported on SBCL currently. It's a specific feature used to restart the webserver without downtime, but in most cases you don't need this."))


#-lispworks
(defmethod start-listening ((acceptor multi-acceptor))
  (when (hunchentoot::acceptor-listen-socket acceptor)
    (hunchentoot-error "acceptor ~A is already listening" acceptor))
  (setf (hunchentoot::acceptor-listen-socket acceptor)
        (cond
          ((listen-fd acceptor)
           (listen-on-fd (listen-fd acceptor) :element-type '(unsigned-byte 8)))
          (t (usocket:socket-listen (or (acceptor-address acceptor)
                                 usocket:*wildcard-host*)
                             (acceptor-port acceptor)
                             :reuseaddress t
                             :backlog (acceptor-listen-backlog acceptor)
                             :element-type '(unsigned-byte 8)))))
  (values))

;; (setf *ma* (make-instance 'multi-acceptor :port 5001))
;; (start *ma*)

(defun add-acceptor (multi-acceptor host acceptor)
  (setf (alexandria:assoc-value
         (sub-acceptors multi-acceptor)
         host :test 'equal)
        acceptor))
