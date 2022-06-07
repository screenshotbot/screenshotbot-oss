(fiasco:define-test-package :hunchensocket-tests
  (:use :cl :hunchensocket :fiasco :alexandria)
  (:import-from :hunchensocket
                #:with-new-client-for-resource
                #:handle-frame
                #:read-frame
                #:read-frame-from-client
                #:read-handle-loop
                #:read-application-data
                #:opcode
                #:state
                #:data
                #:+text-frame+
                #:+binary-frame+
                #:+continuation-frame+
                #:+ping+
                #:+pong+
                #:websocket-error
                #:websocket-error-status
                #:mask-unmask))
(in-package :hunchensocket-tests)


;;; unit tests based on RFC examples
;;;
(defun test-frame-reader-1 (expected-opcode expected-string input)
  (flex:with-input-from-sequence
      (stream input)
    (let ((frame (read-frame stream :read-payload-p t)))
      (with-slots (opcode data) frame
        (is (= opcode expected-opcode))
        (is (string= expected-string
                     (flex:octets-to-string data
                                            :external-format :utf-8)))
        frame))))

(deftest frame-reader ()
  "Test reading unfragmented non-binary frames"
  (loop for (opcode contains input)
          in '(;; A single-frame unmasked text message
               (#.+text-frame+ "Hello" #(#x81 #x05 #x48 #x65
                                         #x6c #x6c #x6f))
               ;; A single-frame masked text message
               (#.+text-frame+ "Hello" #(#x81 #x85 #x37 #xfa
                                         #x21 #x3d #x7f #x9f
                                         #x4d #x51 #x58))
               ;; Unmasked Ping request and masked Ping response
               (#.+ping+       "Hello" #(#x89 #x05 #x48 #x65
                                         #x6c #x6c #x6f))
               (#.+pong+       "Hello" #(#x8a #x85 #x37 #xfa
                                         #x21 #x3d #x7f #x9f
                                         #x4d #x51 #x58))
               ;; Me being clever
               (#.+ping+       "Cello" #(#x89 #x05 #x43 #x65
                                         #x6c #x6c #x6f)))
        do (test-frame-reader-1 opcode contains input)))


;;; This tests a lot more machinery
;;;
(defvar *expected-messages*)
(defvar *expected-files*)

(defclass mock-request ()
  ((headers-in :initform '((:origin . "mock")
                           (:user-agent . "moremock"))
               :reader hunchentoot:headers-in)))

(defclass mock-resource (websocket-resource)
  ()
  (:default-initargs :client-class 'mock-client))

(defclass mock-client (websocket-client)
  ((received-messages :initform nil)
   (received-files    :initform nil)))

(defmethod text-message-received ((resource mock-resource)
                                  (client mock-client) message)
  (is *expected-messages* "Didn't expect a message at all but got one")
  (when *expected-messages*
    (is (string= (pop *expected-messages*) message))
    (with-slots (received-messages) client
      (push message received-messages))))

(defmethod binary-message-received ((resource mock-resource)
                                    (client mock-client) file)
  (is *expected-files*
      "Didn't expect a binary file at all but got one")
  (when *expected-files*
    (let ((file-contents
            (with-open-file (fstream file :direction :input
                                          :element-type '(unsigned-byte 8))
              (let ((seq (make-array (file-length fstream)
                                     :element-type '(unsigned-byte 8)
                                     :fill-pointer t)))
                (setf (fill-pointer seq) (read-sequence seq fstream))
                seq))))
      (is (equalp (pop *expected-files*) file-contents))
      (with-slots (received-files) client
        (push file-contents received-files)))))

(defmacro with-resource-and-client ((res-sym client-sym)
                                    (mock-input mock-output) &body body)
  `(let ((,res-sym (make-instance 'mock-resource)))
     (with-new-client-for-resource
         (,client-sym :input-stream ,mock-input
                      :output-stream ,mock-output
                      :resource ,res-sym
                      :request  (make-instance 'mock-request))
       ,@body)))

(defun test-frame-handler-1 (input &key expected-messages expected-files)
  (flex:with-input-from-sequence (mock-input (apply #'concatenate 'vector
                                                    (ensure-list input)))
    (let ((*expected-messages* expected-messages)
          (*expected-files* expected-files))
      (with-resource-and-client (resource client) (mock-input nil)
        (with-slots (state) client
          (handler-case
              (signals end-of-file
                (loop do (handle-frame resource
                                       client
                                       (read-frame-from-client client))))
            (end-of-file (e) (declare (ignore e)))))
        (with-slots (received-messages received-files) client
          (is (equalp expected-messages (reverse received-messages)))
          (is (equalp expected-files (reverse received-files))))
        client))))

(deftest frame-handler ()
  (loop for (text . vectors)
          in '(;; A single-frame unmasked text message
               (("Hello") #(#x81 #x05 #x48 #x65
                            #x6c #x6c #x6f))
               ;; A single-frame masked text message
               (("Hello") #(#x81 #x85 #x37 #xfa
                            #x21 #x3d #x7f #x9f
                            #x4d #x51 #x58))
               (("Hello")
                ;; contains "Hel"
                #(#x01 #x03 #x48 #x65 #x6c) 
                ;; contains "lo"
                #(#x80 #x02 #x6c #x6f)))
        do (test-frame-handler-1 vectors :expected-messages text)))

(defvar *max-fragment-size* 1024)
(defvar *max-total-size*    4096)

(defmethod check-message ((resource mock-resource)
                          (client mock-client) (opcode (eql +binary-frame+))
                          length total)
  (cond ((> length *max-fragment-size*)
         (websocket-error 1009 "Message fragment too big"))
        ((> total *max-total-size*)
         (websocket-error 1009 "Total message too big"))))

(defmacro signals-with-lambda ((condition-type lambda) &body body)
  `(signals ,condition-type
     (handler-bind
         ((,condition-type ,lambda))
       ,@body)))

(deftest binary-message ()
  (let* ((length 128)
         (42-array (make-array length :initial-element 42))
         (masking-key #(#x01 #x02 #x03 #x04)))
    (test-frame-handler-1 (list #(#x82 #xFE)
                                  (coerce `(,(ash (ldb (byte 8 8) length) 8)
                                            ,(ldb (byte 8 0) length)) 'vector)
                                  masking-key
                                  (mask-unmask
                                   (copy-sequence 'vector 42-array)
                                   masking-key))
                            :expected-files (list 42-array))))

(defun test-fragmented-binaries (&rest first-bytes-of-each-fragment)
  (let* ((42-array (make-array 256 :initial-element 42))
         (masking-key #(#x01 #x02 #x03 #x04)))
    (test-frame-handler-1
     (loop for first-byte in first-bytes-of-each-fragment
           append (list (make-array 1 :initial-element first-byte)
                        #(#xFE #x01 #x00)
                        masking-key
                        (mask-unmask
                         (copy-sequence 'vector 42-array)
                         masking-key)))
     :expected-files (list
                      (apply #'concatenate 'vector
                             (loop repeat
                                   (length first-bytes-of-each-fragment)
                                   collect 42-array))))))

(deftest fragmented-binary-1024 ()
  (test-fragmented-binaries #x02 #x00 #x00 #x80))

(deftest incorrect-fragmentation-1024 ()
  (flet ((check-error (e status regexp)
           (is (= (websocket-error-status e) status))
           (is (ppcre:scan regexp (princ-to-string e)))))
    (signals-with-lambda (websocket-error
                          (alexandria:rcurry #'check-error
                                             1002
                                             "Not discarding initiated fragment sequence"))
      (test-fragmented-binaries #x02 #x00 #x01 #x80))
    (signals-with-lambda (websocket-error
                          (alexandria:rcurry #'check-error
                                             1002
                                             "Unexpected continuation frame"))
      (test-fragmented-binaries #x00 #x00 #x01 #x80))
    (signals-with-lambda (websocket-error
                          (alexandria:rcurry #'check-error
                                             1009
                                             "Total message too big"))
      (let ((*max-total-size* 780)
            (*max-fragment-size* 512))
        (test-fragmented-binaries #x02 #x00 #x00 #x80)))
    (signals-with-lambda (websocket-error
                          (alexandria:rcurry #'check-error
                                             1009
                                             "Message fragment too big"))
      (let ((*max-total-size* 2048)
            (*max-fragment-size* 128))
        (test-fragmented-binaries #x02 #x00 #x00 #x80)))))






  
  





