(in-package :hunchensocket)

(define-constant +websocket-magic-key+
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  :test #'string=
  :documentation "Fixed magic WebSocket UUIDv4 key use in handshakes")

(define-constant +continuation-frame+    #x0)
(define-constant +text-frame+            #x1)
(define-constant +binary-frame+          #x2)
(define-constant +connection-close+      #x8)
(define-constant +ping+                  #x9)
(define-constant +pong+                  #xA)

(defun control-frame-p (opcode)
  (plusp (logand #x8 opcode)))

(defvar *websocket-socket* nil
  "The currently active WebSocket socket")


;;; Mandatory API
;;;
(defvar *websocket-dispatch-table* nil
  "List of handler closures that will be queried for new connections")

(defclass websocket-acceptor (acceptor)
  ((websocket-timeout :initarg :websocket-timeout
                      :accessor websocket-timeout
                      :initform 300
                      :documentation "Custom WebSocket timeout override."))
  (:default-initargs :request-class 'websocket-request
                     :reply-class 'websocket-reply))

(defclass websocket-ssl-acceptor (websocket-acceptor ssl-acceptor) ()
  (:documentation "Special WebSocket SSL acceptor"))

(defclass websocket-client ()
  ((input-stream     :initarg input-stream
                     :initform (error "Must make clients with input streams"))
   (output-stream    :initarg output-stream
                     :initform (error "Must make clients with output streams"))
   (request    :initarg request
               :reader client-request
               :initform (error "Must make clients with requests"))
   (write-lock :initform (make-lock))
   (state      :initform :disconnected)
   (pending-fragments :initform nil)
   (pending-opcode    :initform nil)))

(defmethod initialize-instance :after ((client websocket-client)
                                       &key &allow-other-keys)
  "Allows CLIENT to be passed more keywords on MAKE-INSTANCE.")

(defclass websocket-resource ()
  ((clients :initform nil :reader clients)
   (client-class :initarg :client-class :initform 'websocket-client)
   (lock :initform (make-lock))))

(defmethod print-object ((obj websocket-resource) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (clients client-class) obj
        (format stream "(~a connected clients of class ~a)"
                (length clients)
                client-class))))

(defgeneric text-message-received (resource client message))

(defgeneric binary-message-received  (resource client binary))

;; Optional API
;;
(defgeneric client-connected (resource client)
  (:method (resource client)
    (declare (ignore resource client))))

(defgeneric client-disconnected (resource client)
  (:method (resource client)
    (declare (ignore resource client))))

(defgeneric check-message (resource client opcode fragment-length total-length)
  (:method ((resource websocket-resource)
            (client websocket-client) opcode length total)
    (declare (ignore resource client))
    (cond ((> length #xffff) ; 65KiB
           (websocket-error 1009 "Message fragment too big"))
          ((> total #xfffff) ; 1 MiB
           (websocket-error 1009 "Total message too big"))))
  (:method ((resource websocket-resource)
            (client websocket-client)
            (opcode (eql +binary-frame+)) length total)
    (websocket-error 1003 "Binaries not accepted")))

;; Convenience API
;;
(defun send-text-message (client message)
  "MESSAGE is a string"
  (send-frame client +text-frame+
              (flexi-streams:string-to-octets message
                                              :external-format :utf-8)))

(defun send-binary-message (client message)
  "MESSAGE is an array of octets"
  (send-frame client +binary-frame+
              message))

(defun send-ping (client &optional (message #()))
  (send-frame client +ping+ message))

(defun close-connection (client &key (data nil data-supplied-p)
                                     (status 1000)
                                     (reason "Normal close"))
  (send-frame client
              +connection-close+
              (if data-supplied-p
                  data
                  (concatenate 'vector
                               (coerce (list (logand (ash status -8) #xff)
                                             (logand status #xff))
                                       'vector)
                               (flexi-streams:string-to-octets
                                reason
                                :external-format :utf-8))))
  (setf (slot-value client 'state) :awaiting-close))


(defun send-frame (client opcode data)
  (with-slots (write-lock output-stream) client
    (with-lock-held (write-lock)
      (write-frame output-stream opcode data))))


;;; Request/reply Hunchentoot overrides
;;;
(defclass websocket-request (request)
  ((handler :accessor websocket-resource
            :initform nil
            :documentation "Message handler of the current request")))

(defclass websocket-reply (reply) ())

(defmethod initialize-instance :after ((reply websocket-reply) &rest initargs)
  "Set the reply's external format to Unix EOL / UTF-8 explicitly."
  (declare (ignore initargs))
  (setf (reply-external-format reply)
        (make-external-format :utf8 :eol-style :lf)))


;;; Conditions

(define-condition websocket-error (simple-error)
  ((error-status :initarg :status :reader websocket-error-status
                 :initform nil))
  (:documentation "Superclass for all errors related to Websocket."))

(defun websocket-error (status format-control &rest format-arguments)
  "Signals an error of type HUNCHENTOOT-SIMPLE-ERROR with the provided
format control and arguments."
  (error 'websocket-error
         :status status
         :format-control format-control
         :format-arguments format-arguments))


;;; Client and resource machinery
;;;
(defmethod initialize-instance :after ((resource websocket-resource)
                                       &key client-class)
  (assert (subtypep client-class 'websocket-client)))

(defun call-with-new-client-for-resource (client resource fn)
  (with-slots (clients lock) resource
    (unwind-protect
         (progn
           (bt:with-lock-held (lock)
             (push client clients))
           (setf (slot-value client 'state) :connected)
           (client-connected resource client)
           (funcall fn))
      (bt:with-lock-held (lock)
        (with-slots (write-lock) client
          (bt:with-lock-held (write-lock)
            (setq clients (remove client clients)))))
      (client-disconnected resource client))))

(defmacro with-new-client-for-resource ((client-sym &key input-stream
                                                      output-stream
                                                      resource
                                                      request)
                                        &body body)
  (alexandria:once-only (resource request)
    `(let ((,client-sym (apply #'make-instance
                               (slot-value ,resource 'client-class)
                               'input-stream ,input-stream
                               'output-stream ,output-stream
                               'request ,request
                               :request ,request
                               (loop for (header . value)
                                       in (headers-in ,request)
                                     collect header collect value))))
       (call-with-new-client-for-resource ,client-sym
                                          ,resource
                                          #'(lambda () ,@body)))))

(defun websocket-uri (request host &optional ssl)
  "Form WebSocket URL (ws:// or wss://) URL."
  (format nil "~:[ws~;wss~]://~a~a" ssl host (script-name request)))


;;; Binary reading/writing machinery
;;;
(defun read-unsigned-big-endian (stream n)
  "Read N bytes from stream and return the big-endian number"
  (loop for i from (1- n) downto 0
        sum (* (read-byte stream) (expt 256 i))))

(defun read-n-bytes-into-sequence (stream n)
  "Return an array of N bytes read from stream"
  (let* ((array (make-array n :element-type '(unsigned-byte 8)))
         (read (read-sequence array stream)))
    (assert (= read n) nil
            "Expected to read ~a bytes, but read ~a" n read)
    array))

(defclass frame ()
  ((opcode          :initarg :opcode :accessor frame-opcode)
   (data                             :accessor frame-data)
   (finp            :initarg :finp)
   (payload-length  :initarg :payload-length :accessor frame-payload-length)
   (masking-key     :initarg :masking-key)))

(defun read-frame (stream &key read-payload-p)
  (let* ((first-byte       (read-byte stream))
         (fin              (ldb (byte 1 7) first-byte))
         (extensions       (ldb (byte 3 4) first-byte))
         (opcode           (ldb (byte 4 0) first-byte))
         (second-byte      (read-byte stream))
         (mask-p           (plusp (ldb(byte 1 7) second-byte)))
         (payload-length   (ldb (byte 7 0) second-byte))
         (payload-length   (cond ((<= 0 payload-length 125)
                                  payload-length)
                                 (t
                                  (read-unsigned-big-endian
                                   stream (case payload-length
                                            (126 2)
                                            (127 8))))))
         (masking-key      (if mask-p (read-n-bytes-into-sequence stream 4)))
         (extension-data   nil))
    (declare (ignore extension-data))
    (when (and (control-frame-p opcode)
               (> payload-length 125))
      (websocket-error
       1002 "Control frame is too large" extensions))
    (when (plusp extensions)
      (websocket-error
       1002 "No extensions negotiated, but client sends ~a!" extensions))
    (let ((frame
            (make-instance 'frame :opcode opcode
                                  :finp (plusp fin)
                                  :masking-key masking-key
                                  :payload-length payload-length)))
      (when (or (control-frame-p opcode)
                read-payload-p)
        (read-application-data stream frame))
      frame)))

(defun read-frame-from-client (client)
  "Read a text or binary message from CLIENT."
  (with-slots (input-stream) client
    (read-frame input-stream)))

(defun mask-unmask (data masking-key)
  ;; RFC6455 Masking
  ;;
  ;; Octet i of the transformed data
  ;; ("transformed-octet-i") is the XOR of octet i
  ;; of the original data ("original-octet-i")
  ;; with octet at index i modulo 4 of the masking
  ;; key ("masking-key-octet-j"):
  (loop for i from 0 below (length data)
        do (setf (aref data i)
                 (logxor (aref data i)
                         (aref masking-key
                               (mod i 4)))))
  data)

(defun read-application-data (stream frame)
  (with-slots (masking-key payload-length data) frame
    (setq data (read-n-bytes-into-sequence stream
                                           payload-length))
    (when masking-key
      (mask-unmask data masking-key))))

(defun write-frame (stream opcode &optional data)
  (let* ((first-byte     #x00)
         (second-byte    #x00)
         (len            (if data (length data) 0))
         (payload-length (cond ((<= len 125)        len)
                               ((< len (expt 2 16)) 126)
                               (t                   127)))
         (mask-p         nil))
    (setf (ldb (byte 1 7) first-byte)  1
          (ldb (byte 3 4) first-byte)  0
          (ldb (byte 4 0) first-byte)  opcode
          (ldb (byte 1 7) second-byte) (if mask-p 1 0)
          (ldb (byte 7 0) second-byte) payload-length)
    (write-byte first-byte stream)
    (write-byte second-byte stream)
    (loop for i from  (1- (cond ((= payload-length 126) 2)
                                ((= payload-length 127) 8)
                                (t                      0)))
          downto 0
          for out = (ash len (- (* 8 i)))
          do (write-byte (logand out #xff) stream))
    ;; (if mask-p
    ;;     (error "sending masked messages not implemented yet"))
    (if data (write-sequence data stream))
    (force-output stream)))


;;; State machine and main websocket loop
;;;
(defun handle-frame (resource client frame)
  (with-slots (state pending-fragments pending-opcode input-stream) client
    (with-slots (opcode finp payload-length masking-key) frame
      (flet ((maybe-accept-data-frame ()
               (check-message resource client (or pending-opcode
                                                  opcode)
                              payload-length
                              (+ payload-length
                                 (reduce #'+ (mapcar
                                              #'frame-payload-length
                                              pending-fragments))))
               (read-application-data input-stream frame)))
        (cond
          ((eq +pong+ opcode)
           ;; Probably just a heartbeat, don't do anything.
           )
          ((eq :awaiting-close state)
           ;; We're waiting a close because we explicitly sent one to the
           ;; client. Error out if the next message is not a close.
           ;;
           (unless (eq opcode +connection-close+)
             (websocket-error
              1002 "Expected connection close from client, got 0x~x" opcode))
           (setq state :closed))
          ((not finp)
           ;; This is a non-FIN fragment Check opcode, append to client's
           ;; fragments.
           ;;
           (cond ((and (= opcode +continuation-frame+)
                       (not pending-fragments))
                  (websocket-error
                   1002 "Unexpected continuation frame"))
                 ((control-frame-p opcode)
                  (websocket-error
                   1002 "Control frames can't be fragmented"))
                 ((and pending-fragments
                       (/= opcode +continuation-frame+))
                  (websocket-error
                   1002 "Not discarding initiated fragment sequence"))
                 (t
                  ;; A data frame, is either initiaing a new fragment sequence
                  ;; or continuing one
                  ;;
                  (maybe-accept-data-frame)
                  (cond ((= opcode +continuation-frame+)
                         (push frame pending-fragments))
                        (t
                         (setq pending-opcode opcode
                               pending-fragments (list frame)))))))
          ((and pending-fragments
                (not (or (control-frame-p opcode)
                         (= opcode +continuation-frame+))))
           ;; This is a FIN fragment and (1) there are pending fragments and (2)
           ;; this isn't a control or continuation frame. Error out.
           ;;
           (websocket-error
            1002 "Only control frames can interleave fragment sequences."))
          (t
           ;; This is a final, FIN fragment. So first read the fragment's data
           ;; into the `data' slot.
           ;;
           (cond
             ((not (control-frame-p opcode))
              ;; This is either a single-fragment data frame or a continuation
              ;; frame. Join the fragments and keep on processing. Join any
              ;; outstanding fragments and process the message.
              ;;
              (maybe-accept-data-frame)
              (unless pending-opcode
                (setq pending-opcode opcode))
              (let ((ordered-frames
                      (reverse (cons frame pending-fragments))))
                (cond ((eq +text-frame+ pending-opcode)
                       ;; A text message was received
                       ;;
                       (text-message-received
                        resource client
                        (flexi-streams:octets-to-string
                         (apply #'concatenate 'vector
                                (mapcar #'frame-data
                                        ordered-frames))
                         :external-format :utf-8)))
                      ((eq +binary-frame+ pending-opcode)
                       ;; A binary message was received
                       ;;
                       (let ((temp-file
                               (fad:with-output-to-temporary-file
                                   (fstream :element-type '(unsigned-byte 8))
                                 (loop for fragment in ordered-frames
                                       do (write-sequence (frame-data frame)
                                                          fstream)))))
                         (unwind-protect
                              (binary-message-received resource client
                                                       temp-file)
                           (delete-file temp-file))))
                      (t
                       (websocket-error
                        1002 "Client sent unknown opcode ~a" opcode))))
              (setf pending-fragments nil))
             ((eq +ping+ opcode)
              ;; Reply to client-initiated ping with a server-pong with the
              ;; same data
              (send-frame client +pong+ (frame-data frame)))
             ((eq +connection-close+ opcode)
              ;; Reply to client-initiated close with a server-close with the
              ;; same data
              ;;
              (close-connection client :data (frame-data frame))
              (setq state :closed))
             (t
              (websocket-error
               1002 "Client sent unknown opcode ~a" opcode)))))))))


(defun read-handle-loop (resource client
                         &optional (version :rfc-6455))
  "Implements the main WebSocket loop for supported protocol
versions. Framing is handled automatically, CLIENT handles the actual
payloads."
  (ecase version
    (:rfc-6455
     (handler-bind ((websocket-error
                      #'(lambda (error)
                          (with-slots (error-status format-control format-arguments)
                              error
                            (close-connection
                             client
                             :status error-status
                             :reason (princ-to-string error)))))
                    (flexi-streams:external-format-error
                      #'(lambda (e)
                          (declare (ignore e))
                          (close-connection client :status 1007
                                                   :reason "Bad UTF-8")))
                    (error
                      #'(lambda (e)
                          (declare (ignore e))
                          (close-connection client :status 1011
                                                   :reason "Internal error"))))
       (with-slots (state) client
         (loop do (handle-frame resource
                                client
                                (read-frame-from-client client))
               while (not (eq :closed state))))))))


;;; Hook onto normal Hunchentoot processing
;;;
;;; The `:after' specilization of `process-request' will happen after
;;; the main Hunchentoot one. It is hunchentoot which eventually calls
;;; our specialization of `acceptor-dispatch-request', who will, in
;;; turn, try to figure out if the client is requesting
;;; websockets. Hunchentoot's `process-request' will also eventually
;;; reply to the client. In the `:after' specialization we might enter
;;; into `read-handle-loop' and thus keep the socket alive. That happens
;;; if:
;;;
;;; 1. There are suitable "Connection" and "Upgrade" headers and
;;;    `websocket-resource' object is found for request.
;;;
;;; 2. The websocket handshake completes sucessfully, whereby the
;;;    callees of `acceptor-dispatch-request' will have set
;;;    `+http-switching-protocols+' accordingly.
;;;
;;; If any of these steps fail, errors might be signalled, but normal
;;; hunchentoot processing of the HTTP request still happens.

(defmethod process-connection :around ((*acceptor* websocket-acceptor)
                                       (socket t))
  "Sprinkle the current connection with dynamic bindings."
  (let ((*websocket-socket* socket))
    (call-next-method)))

(defmethod process-request :after ((request websocket-request))
  "After HTTP processing REQUEST, maybe hijack into WebSocket loop."
  ;; HACK! ask upstream Hunchentoot for this.
  (let ((stream (hunchentoot::content-stream request)))
    (when (= +http-switching-protocols+ (return-code*))
      (force-output stream)
      (let* ((timeout (websocket-timeout (request-acceptor request)))
             (resource (websocket-resource request)))
        (with-new-client-for-resource (client :input-stream stream
                                              :output-stream stream
                                              :resource resource
                                              :request request)
          ;; See https://github.com/joaotavora/hunchensocket/pull/23
          ;; LispWorks in Hunchentoot passes around a USOCKET:USOCKET
          ;; handle, not an actual such object. Unfortunately, abusing
          ;; Hunchentoot's internals consequently forces us to tend to
          ;; this LispWorks particularity.
          #-lispworks
          (hunchentoot::set-timeouts *websocket-socket* timeout timeout)
          #+lispworks
          (setf (stream:stream-read-timeout stream) timeout
                (stream:stream-write-timeout stream) timeout)

          (catch 'websocket-done
            (handler-bind ((error #'(lambda (e)
                                      (maybe-invoke-debugger e)
                                      (log-message* :error "Error: ~a" e)
                                      (throw 'websocket-done nil))))
              (read-handle-loop resource client))))))))

(defmethod handle-handshake ((acceptor websocket-acceptor) request reply)
  "Analyse REQUEST for WebSocket handshake.

Destructively modify REPLY accordingly in case of success, exit
non-locally with an error instead."
  ;; Implements 4.2.2.  Sending the Server's Opening Handshake
  (let ((requested-version (header-in* :sec-websocket-version request)))
    (cond ((not (equal "13" requested-version))
           (websocket-error 1002
            "Unsupported websocket version ~a" requested-version))
          ((header-in :sec-websocket-draft request)
           (websocket-error 1002
            "Websocket draft is unsupported"))
          ((header-in :sec-websocket-key request)
           (let ((sec-websocket-key+magic
                   (concatenate 'string (header-in :sec-websocket-key request)
                                "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))
             (setf (header-out :sec-websocket-accept reply)
                   (base64:usb8-array-to-base64-string
                    (ironclad:digest-sequence
                     'ironclad:sha1
                     (ironclad:ascii-string-to-byte-array
                      sec-websocket-key+magic))))
             (setf (header-out :sec-websocket-origin reply)
                   (header-in :origin request))
             (setf (header-out :sec-websocket-location reply)
                   (or (websocket-uri request (header-in :host request)
                                      (ssl-p (request-acceptor request)))))
             (setf (header-out :sec-websocket-protocol reply)
                   (first (split "\\s*,\\s*" (header-in :sec-websocket-protocol
                                                        request))))
             ;; A (possibly empty) list representing the
             ;; protocol-level extensions the server is ready to use.
             ;;
             (setf (header-out :sec-websocket-extensions reply) nil)
             (setf (return-code* reply) +http-switching-protocols+
                   (header-out :upgrade reply) "WebSocket"
                   (header-out :connection reply) "Upgrade"
                   (content-type* reply) "application/octet-stream")
             ;; HACK! but a decent one I think. Notice that we set both
             ;; in and out "Connection" headers to "Upgrade". The out
             ;; header is per RFC, but the in-header is for a different
             ;; reason. If the client additionally send "Keep-Alive" in
             ;; that header, hunchentoot will eventually take it as a
             ;; reason to clobber our out-header value of "Upgrade" with
             ;; "Keep-Alive <timeout>".
             (setf (cdr (find :connection (headers-in request) :key #'car))
                   "Upgrade")))
          (t (websocket-error 1002 "Unsupported unknown websocket version")))))

(defun find-websocket-resource (request)
  "Find the resource for REQUEST by looking up *WEBSOCKET-DISPATCH-TABLE*."
  (some #'(lambda (dispatcher)
            (funcall dispatcher request))
        *websocket-dispatch-table*))

(defmethod acceptor-dispatch-request ((acceptor websocket-acceptor)
                                      (request websocket-request))
  "Attempt WebSocket connection, else fall back to HTTP"
  (cond ((and (member "upgrade" (split "\\s*,\\s*" (header-in* :connection))
                      :test #'string-equal)
              (string= "websocket" (string-downcase (header-in* :upgrade))))
         (cond ((setf (websocket-resource *request*)
                      (find-websocket-resource *request*))
                ;; Found the websocket resource
                (handle-handshake acceptor *request* *reply*)
                ;; HACK! the empty string is also important because if there's
                ;; no content Hunchentoot will declare the connection closed and
                ;; set "Connection: Closed". But there can't be any actual
                ;; content since otherwise it will piggyback onto the first
                ;; websocket frame, which is interpreted as invalid by the
                ;; client. It's also forbidden by the HTTP RFC2616:
                ;;
                ;;    [...] All 1xx (informational), 204 (no content), and 304
                ;;    (not modified) responses MUST NOT include a
                ;;    message-body. [...]
                ;;
                ;; There is a slight non-conformance here: this trick makes
                ;; Hunchentoot send "Content-length: 0". Most browsers don't
                ;; seem to care, but RFC2616 kind of implies that is forbidden,
                ;; since it says the
                ;;
                ;;    [...] the presence of a message-body is signaled by the
                ;;    inclusion of a Content-Length or Transfer-Encoding header
                ;;    field in the requests's message-headers [...]
                ;;
                ;; Note however, we're sending a response, not a request.
                ;;
                (values "" nil nil))
               (t
                ;; Didn't find the websocket-specific resource, return 404.
                (setf (return-code *reply*) +http-not-found+)
                (values nil nil nil))))
        (t
         ;; Client is not requesting websockets, let Hunchentoot do its HTTP
         ;; thing undisturbed.
         (call-next-method))))

;; Local Variables:
;; coding: utf-8-unix
;; End:
