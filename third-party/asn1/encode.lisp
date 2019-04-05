(defpackage #:asn1/encode
  (:use #:cl)
  (:import-from #:fast-io
                #:with-fast-output
                #:fast-write-byte
                #:fast-write-sequence)
  (:import-from #:ironclad)
  (:export #:encode))
(in-package #:asn1/encode)

(declaim (type (simple-array symbol (29)) +tag-types+))
(defparameter +tag-types+
  #(nil
    :boolean
    :integer
    :bit-string
    :octet-string
    :null
    :object-identifier
    :object-descriptor
    :external
    :real
    :enumerated
    nil
    :utf8-string
    nil
    nil
    nil
    :sequence
    :set
    :numeric-string
    :printable-string
    :teletex-string
    :videotext-string
    :ia5-string
    :utc-time
    :generalized-time
    :graphic-string
    :visible-string
    :general-string
    :character-string))

(defparameter *buffer* nil)

(defun oid-to-octets (oid)
  (check-type oid vector)
  (coerce
   (cons
    (+ (* (aref oid 0) 40)
       (aref oid 1))
    (labels ((to-chunk (i)
               (multiple-value-bind (quotinent remainder)
                   (truncate i 128)
                 (cond
                   ((<= 128 quotinent)
                    (let* ((data (to-chunk quotinent))
                           (last (last data)))
                      (rplaca last (+ (car last) 128))
                      (rplacd last (list remainder))
                      data))
                   ((= quotinent 0) (list remainder))
                   (t (list (+ 128 quotinent) remainder))))))
      (loop for i from 2 below (length oid)
            append (to-chunk (aref oid i)))))
   '(simple-array (unsigned-byte 8) (*))))

(defun length-to-octets (len)
  (with-fast-output (buffer)
    (if (< len 128)
        (fast-write-byte len buffer)
        (let ((octets (ironclad:integer-to-octets len)))
          (fast-write-byte (+ (length octets) 128) buffer)
          (fast-write-sequence octets buffer)))))

(defun write-block (asn1 buffer)
  (check-type asn1 cons)
  (let* ((is-component (consp (cdr asn1)))
         (tag (car asn1))
         (tag-num (if (integerp tag)
                      tag
                      (position tag +tag-types+ :test 'eq))))
    (unless tag-num
      (error "Invalid tag: ~A" tag))

    (let ((1st-byte (+ (logand tag-num 31)
                       (if is-component
                           32
                           0))))
      (fast-write-byte 1st-byte buffer)
      (if is-component
          (let ((data (encode (cdr asn1))))
            (fast-write-sequence
             (length-to-octets (length data))
             buffer)
            (fast-write-sequence data buffer))
          (let ((data (case tag
                        (:integer (ironclad:integer-to-octets (cdr asn1)))
                        (:sequence)
                        (:bit-string
                         (let ((res (make-array (1+ (length (cdr asn1)))
                                                :element-type '(unsigned-byte 8)
                                                :initial-element 0)))
                           (replace res (cdr asn1) :start1 1)
                           res))
                        (:octet-string (cdr asn1))
                        (:object-identifier
                         (oid-to-octets (cdr asn1)))
                        (:boolean (make-array 1 :element-type '(unsigned-byte 8)
                                                :initial-contents (list (if (cdr asn1)
                                                                            1
                                                                            0))))
                        (:null (make-array 0 :element-type '(unsigned-byte 8)))
                        (otherwise
                         (cdr asn1)))))
            (fast-write-sequence
             (length-to-octets (length data))
             buffer)
            (fast-write-sequence data buffer))))))

(defun encode (asn1)
  (with-fast-output (buffer)
    (dolist (kv asn1)
      (write-block kv buffer))))
