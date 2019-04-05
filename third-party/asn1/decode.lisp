(defpackage #:asn1/decode
  (:use #:cl)
  (:export #:decode))
(in-package #:asn1/decode)

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

(deftype octets (&optional (len '*)) `(simple-array (unsigned-byte 8) (,len)))

(declaim (ftype (function (octets fixnum fixnum) integer) bytes-to-integer))
(defun bytes-to-integer (data start end)
  (declare (optimize speed))
  (loop with result = 0
        for p from start below end
        for byte = (aref data p)
        do (setf result (+ (* result 256) byte))
        finally (return result)))

(declaim (ftype (function (octets fixnum fixnum) (simple-array integer (*))) bytes-to-oid))
(defun bytes-to-oid (data start end)
  (declare (optimize speed))
  (let ((1st-byte (aref data start)))
    (concatenate 'vector
                 (multiple-value-list
                  (truncate 1st-byte 40))
                 (loop with p = (1+ start)
                       collect
                       (loop with result = 0
                             for id = (aref data p)
                             while (< 128 id)
                             do (setf result (+ (* result 128) (logand id 127)))
                                (incf p)
                             finally (return (+ (* result 128) id)))
                       do (incf p)
                       while (< p end)))))

(defun read-block (data &key (start 0))
  (declare (optimize speed))
  (declare (type octets data)
           (type fixnum start))
  (let ((p start))
    (declare (type fixnum p))
    (let* ((1st-byte (aref data p))
           (is-component (not (zerop (logand 1st-byte 32))))
           (tag-class (ash 1st-byte -6))
           (tag-num (logand 1st-byte 31))
           (tag
             (ecase tag-class
               (0 (and (< tag-num (length +tag-types+))
                       (aref +tag-types+ tag-num)))
               ;; context
               (2 tag-num))))
      (unless tag
        (warn "Unknown tag: ~S (~S)" tag-num (subseq data p)))
      (incf p)
      (let ((2nd-byte (aref data p)))
        (incf p)
        (cond
          ((zerop (logand 2nd-byte 128))
           (let ((end (+ p 2nd-byte)))
             (declare (type fixnum end))
             (values p end tag is-component)))
          (t
           (let* ((len-bytes (logand 2nd-byte 127))
                  (len (bytes-to-integer data p (+ len-bytes p))))
             (declare (type fixnum len-bytes len))
             (incf p len-bytes)
             (let ((end (+ p len)))
               (declare (type fixnum end))
               (values p end tag is-component)))))))))

(defun decode (data &key (start 0) (end (length data)))
  (declare (optimize speed))
  (declare (type octets data)
           (type fixnum start end))
  (multiple-value-bind (chunk-start chunk-end tag recursivep)
      (read-block data :start start)
    (declare (type fixnum chunk-start chunk-end))
    (cons
     (cons tag
           (if recursivep
               (decode data :start chunk-start :end chunk-end)
               (case tag
                 (:integer
                  (bytes-to-integer data chunk-start chunk-end))
                 (:sequence
                  (decode data :start chunk-start :end chunk-end))
                 (:bit-string
                  (let ((unused-bits (aref data chunk-start))
                        (res (subseq data (1+ chunk-start) chunk-end)))
                    (unless (= unused-bits 0)
                      (setf (aref res (1- chunk-end))
                            (logxor (aref res (1- chunk-end))
                                    (1- (expt 2 unused-bits)))))
                    res))
                 (:octet-string
                  (subseq data chunk-start chunk-end))
                 (:object-identifier
                  (bytes-to-oid data chunk-start chunk-end))
                 (:boolean
                  (unless (= 1 (- chunk-end chunk-start))
                    (error "Too long boolean"))
                  (/= 0 (aref data chunk-start)))
                 (:null)
                 (otherwise
                  (subseq data chunk-start chunk-end)))))
     (when (< chunk-end end)
       (decode data :start chunk-end :end end)))))
