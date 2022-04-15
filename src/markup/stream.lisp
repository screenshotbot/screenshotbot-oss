(uiop:define-package #:markup/stream
    (:use :cl)
  (:export #:wrap-stream))
(in-package #:markup/stream)

(defclass markup-stream (trivial-gray-streams:fundamental-character-stream)
  ((delegate
    :initarg :delegate
    :reader stream-delegate)
   (unread-char
    :initform nil
    :accessor %unread-char)
   (history
    :reader history-stream
    :initform (make-string-output-stream))))

(defmethod trivial-gray-streams:stream-read-char ((stream markup-stream))
  (with-slots (unread-char)  stream
   (cond
     (unread-char
      (let ((ret unread-char))
        (setf unread-char nil)
        ret))
     (t
      (let ((ch (read-char (stream-delegate stream) nil :eof)))
        (write-char ch (history-stream stream))
        ch)))))

(defmethod trivial-gray-streams:stream-unread-char ((stream markup-stream) ch)
  (assert (eql nil (%unread-char stream)))
  (setf (%unread-char stream) ch))

(defmethod trivial-gray-streams:stream-peek-char ((stream markup-stream))
  (let ((ret (trivial-gray-streams:stream-read-char stream)))
    (trivial-gray-streams:stream-unread-char stream ret)
    ret))

(defun wrap-stream (stream)
  (make-instance 'markup-stream
                 :delegate stream))

(defun read-so-far (stream)
  (let ((resp (get-output-stream-string (history-stream stream))))
    ;; get-output-stream-string clears up the stream so let's write it
    ;; back
    (write-string resp (history-stream stream))
    (cond
      ((%unread-char stream)
       (assert (> (length resp) 0))
       (str:substring 0 (- (length resp) 1) resp))
      (t resp))))
