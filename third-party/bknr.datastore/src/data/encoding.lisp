;;;; Reading and writing Lisp objects in a binary format.

;;; Design:
;;;
;;;   - compact storage requirements
;;;   - no arbitary limits (e.g. integers may be arbitarily large)
;;;   - high read and write performance, thus no checking for cyclic data

;;; For every supported data type, a character is defined as tag denoting
;;; the type when reading.
;;;
;;; The functions ENCODE and DECODE encode and decode an arbitary object.
;;; Upon write, ENCODE determines the data type from the lisp data type.
;;; Upon read, DECODE determines the data type of the object by looking at
;;; the tag character.
;;;
;;; If the data type is known upfron, the respective coder function can be
;;; called directly.  ENCODE-INTEGER encodes an integer, for example.
;;;
;;; At certain file positions, only one datatype makes sense (i.e. when
;;; writing a structure with a fixed layout).  In this case, the tag need
;;; not be written.  For this purpose, a low-level function, i.e.
;;; %ENCODE-INTEGER, exists to write the object without writing a tag,
;;; and a matching decode function DECODE-INTEGER to read such untagged
;;; data.

;;; Format:

;;;    Field    Format     Comment
;;; ----------------------------------------------------------------
;;; Integer
;;;     tag     #\i
;;;     n       byte       Number of bytes that follow
;;;     data    byte[n]    The actual data, a big endian number
;;;
;;; ----------------------------------------------------------------
;;; Rational
;;;     tag     #\r
;;;     n       byte       Number of bytes that follow
;;;     data    byte[n]    The numerator, a big endian number
;;;     n       byte       Number of bytes that follow
;;;     data    byte[n]    The denominator, a big endian number
;;;
;;; ----------------------------------------------------------------
;;; Reference to a STORE-OBJECT
;;;     tag     #\o
;;;     ID      %integer   ID of the referenced object
;;;
;;; ----------------------------------------------------------------
;;; List
;;;     tag     #\l
;;;     n       %integer   Number of bytes that follow
;;;     data    object[n]  Objects including tag
;;;     tail    object     If n != 0: CDR of the last cons
;;;
;;; ----------------------------------------------------------------
;;; Char
;;;     tag     #\c
;;;     data    char       Character, written with WRITE-CHAR
;;; ----------------------------------------------------------------
;;; String
;;;     tag     #\s
;;;     n       %integer   Number of bytes that follow
;;;     data    char[n]    Characters, written with WRITE-CHAR
;;; Note that the layout of strings will change to not use WRITE-CHAR
;;;
;;; ----------------------------------------------------------------
;;; Symbol
;;;     tag     #\y
;;;     package %string    Name of the home package of the symbol
;;;     name    %string    Name of the symbol
;;;
;;; ----------------------------------------------------------------
;;; Hash-Table
;;;     tag     #\#
;;;     test    %symbol    hash-table-test-function
;;;     r.-size %double    hash-table-rehash-size
;;;     n       %integer   Number of value pairs that follow
;;;     data    pair[n]    Value pairs in the following format
;;;
;;;   pair:
;;;     key     object     Objekt with tag
;;;     value   object     Objekt with tag
;;;
;;; ----------------------------------------------------------------
;;; Single-Float
;;;     tag     #\f
;;;     data    byte[4]    IEEE representation (big endian)
;;;
;;; ----------------------------------------------------------------
;;; Double-Float
;;;     tag     #\d
;;;     data    byte[8]    IEEE representation (big endian)
;;;
;;; ----------------------------------------------------------------
;;; Array (saves all standard array attributes except for displacedness)
;;;     tag     #\a
;;;     type    %symbol           ARRAY-ELEMENT-TYPE
;;;     type-list %list
;;;               present only if %symbol is 'new-list-type, if so it's
;;;               the actualy ARRAY-ELEMENT-TYPE to use.
;;;     flags   byte
;;;               bit 0: array is a vector
;;;               bit 1: array is adjustable
;;;               bit 2: vector has a fill-pointer
;;;               other bits reserved
;;;
;;;   if (flags has bit 0 set) {
;;;     length  %integer          Length of fector
;;;   } else {
;;;     n       %integer          Number of dimensions following
;;;     dims    %integer[n]       ARRAY-DIMENSIONS
;;;   }
;;;
;;;   if (flags has bit 2 set) {
;;;     fp      %integer          fill-pointer
;;;   }
;;;
;;;     data    object[\Pi dims]  Data in row-major-order
;;;
;;; ----------------------------------------------------------------

(in-package :bknr.datastore)

;;;; workaround

(declaim (inline %read-char %write-char))
(defun %read-char (stream)
  (code-char (%decode-uint32 stream)))

(defun %write-char (char stream)
  (%encode-int32 (char-code char) stream))

;;;; tags
(declaim (inline %read-tag %write-tag))
(defun %read-tag (stream &optional (eof-error-p t) eof-value)
  (let ((b (read-byte stream eof-error-p -1)))
    (if (= b -1)
        eof-value
        (code-char b))))

(defun %write-tag (char stream)
  (write-byte (char-code char) stream))

;;;; binary encoding

(defun %encode-int32 (object stream)
  (write-byte (ldb (byte 8 24) object) stream)
  (write-byte (ldb (byte 8 16) object) stream)
  (write-byte (ldb (byte 8 08) object) stream)
  (write-byte (ldb (byte 8 00) object) stream))

(defun %encode-int16 (object stream)
  (write-byte (ldb (byte 8 08) object) stream)
  (write-byte (ldb (byte 8 00) object) stream))

(defun %encode-integer (object stream)
  (let ((n (ceiling (1+ (integer-length object)) 8)))
    (write-byte n stream)
    (loop
       for i from (- (* n 8) 8) downto 0 by 8
       do (write-byte (ldb (byte 8 i) object) stream))))

(defun %encode-rational (object stream)
  (%encode-integer (numerator object) stream)
  (%encode-integer (denominator object) stream))

(defun encode-integer (object stream)
  (%write-tag #\i stream)
  (%encode-integer object stream))

(defun encode-rational (object stream)
  (%write-tag #\r stream)
  (%encode-rational object stream))

(defun count-conses (list)
  ;; Vorsicht, CMUCL LOOP hat einen Bug mit dotted lists.
  ;; Daher nicht FOR-ON verwenden.
  (loop for l = list then (cdr l)
     while (consp l)
     count 1))

(defun %encode-list (object stream)
  (let ((len (count-conses object)))
    (%encode-integer len stream)
    ;; Vorsicht, CMUCL LOOP hat einen Bug mit dotted lists.
    ;; Daher nicht FOR-ON verwenden.
    (when (> len 0)
      (loop for l = object then (cdr l)
         while (consp l)
         do (encode (car l) stream)
         finally (encode l stream)))))

(defun encode-list (object stream)
  (%write-tag #\l stream)
  (%encode-list object stream))

(defun encode-char (object stream)
  (%write-tag #\c stream)
  (%write-char object stream))

(defun %encode-string (object stream)
  (let ((octets (trivial-utf-8:string-to-utf-8-bytes object)))
    (%encode-integer (length octets) stream)
    (write-sequence octets stream)))

(defun encode-string (object stream)
  (%write-tag #\s stream)
  (%encode-string object stream))

(defun %encode-symbol (object stream)
  (%encode-string (package-name (symbol-package object)) stream)
  (%encode-string (symbol-name object) stream))

(defun encode-symbol (object stream)
  (%write-tag #\y stream)
  (%encode-symbol object stream))

(defun encode-hash-table (object stream)
  (%write-tag #\# stream)
  (%encode-symbol (hash-table-test object) stream)
  (%encode-double-float (float (hash-table-rehash-size object) 1.0d0) stream)
  (%encode-integer (hash-table-count object) stream)
  (maphash (lambda (k v)
             (encode k stream)
             (encode v stream))
           object))

(defun %encode-single-float (object stream)
  #+allegro
  (map nil #'(lambda (short)
               (%encode-int16 short stream))
       (multiple-value-list (excl::single-float-to-shorts object)))
  #+cmu
  (%encode-int32 (kernel:single-float-bits object) stream)
  #+openmcl
  (%encode-int32 (ccl::single-float-bits object) stream)
  #+sbcl
  (%encode-int32 (sb-kernel:single-float-bits object) stream)
  #+lispworks
  (%encode-int32 (float-features:single-float-bits object) stream))

(defun encode-single-float (object stream)
  (%write-tag #\f stream)
  (%encode-single-float object stream))

(defun %encode-double-float (object stream)
  #+cmucl
  (map nil #'(lambda (short)
               (%encode-int16 short stream))
       (multiple-value-list (excl::double-float-to-shorts object)))
  #+cmu
  (progn (%encode-int32 (kernel:double-float-high-bits object) stream)
         (%encode-int32 (kernel:double-float-low-bits object) stream))
  #+openmcl
  (multiple-value-bind (hi lo) (ccl::double-float-bits object)
    (%encode-int32 hi stream)
    (%encode-int32 lo stream))
  #+sbcl
  (progn (%encode-int32 (sb-kernel:double-float-high-bits object) stream)
         (%encode-int32 (sb-kernel:double-float-low-bits object) stream))
  #+lispworks
  (let* ((int (float-features:double-float-bits object)))
    (%encode-int32 (ldb (byte 32 32) int) stream)
    (%encode-int32 (ldb (byte 32 0) int) stream)))

(defun encode-double-float (object stream)
  (%write-tag #\d stream)
  (%encode-double-float object stream))

(defun %encode-array (object stream)
  (let ((type (array-element-type object)))
   (cond
     ((symbolp type)
      (%encode-symbol type stream))
     (type
      ;; backward compatiblity for types that are not symbols
      (%encode-symbol 'new-list-type stream)
      (%encode-list type stream))))
  (let* ((vectorp (typep object 'vector))
         (fill-pointer-p (array-has-fill-pointer-p object))
         (flags (logior (if vectorp 1 0)
                        (if (adjustable-array-p object) 2 0)
                        (if fill-pointer-p 4 0)))
         (dims (array-dimensions object)))
    (write-byte flags stream)
    (cond
      (vectorp
       (%encode-integer (car dims) stream))
      (t
       (%encode-integer (length dims) stream)
       (dolist (d dims)
         (%encode-integer d stream))))
    (when fill-pointer-p
      (%encode-integer (fill-pointer object) stream))
    (dotimes (i (reduce #'* dims))
      (encode (row-major-aref object i) stream))))

(defun encode-array (object stream)
  (%write-tag #\a stream)
  (%encode-array object stream))

(defun encode (object stream)
  (typecase object
    (integer (encode-integer object stream))
    (rational (encode-rational object stream))
    (symbol (encode-symbol object stream))
    (character (encode-char object stream))
    (string (encode-string object stream))
    (list (encode-list object stream))
    (array (encode-array object stream))
    (hash-table (encode-hash-table object stream))
    (single-float (encode-single-float object stream))
    (double-float (encode-double-float object stream))
    (t (encode-object object stream))))

(defgeneric encode-object (object stream))

;;;; decoding

(defun %decode-integer/fixed (stream n)
  (let* ((initial (read-byte stream))
         (result (if (logbitp 7 initial) -1 0)))
    (setf result (logior (ash result 8) initial))
    (dotimes (x (1- n))
      (setf result (logior (ash result 8) (read-byte stream))))
    result))

(defun %decode-uint16 (stream)
  (logior (ash (read-byte stream) 08)
          (read-byte stream)))

(defun %decode-sint32 (stream)
  (%decode-integer/fixed stream 4))

(defun %decode-uint32 (stream)
  (logior (ash (read-byte stream) 24)
          (ash (read-byte stream) 16)
          (ash (read-byte stream) 08)
          (read-byte stream)))

(defun %decode-integer (stream)
  (let ((n (read-byte stream)))
    (assert (plusp n))                  ;n==0 geben wir nicht aus
    (%decode-integer/fixed stream n)))

(defun %decode-rational (stream)
  (/ (%decode-integer stream)
     (%decode-integer stream)))

(defun %decode-char (stream)
  (%read-char stream))

(defvar *all-strings* (make-hash-table :test #'equal))

(defun dedup-string (str)
  (let ((existing (gethash str *all-strings*)))
    (or
     existing
     (setf (gethash str *all-strings*) str))))

(defun %decode-string (stream)
  (dedup-string
   (labels ((octets-to-string-safe (octets) ; safe and portable
              (let ((flexi-streams:*substitution-char* #\?))
                (handler-case
                    (flexi-streams:octets-to-string octets :external-format :utf-8)
                  (flexi-streams:external-format-condition (e)
                    (declare (ignore e))
                    (let ((string (flexi-streams:octets-to-string octets :external-format :ascii)))
                      (warn "could not decode string ~S as utf-8, decoded as ASCII" string)
                      string)))))
            (octets-to-string (octets)
              (handler-case
                  (trivial-utf-8:utf-8-bytes-to-string octets)
                (trivial-utf-8:utf-8-decoding-error ()
                  (octets-to-string-safe octets)))))
     (let* ((n (%decode-integer stream))
            (buffer (make-array n :element-type '(unsigned-byte 8))))
       (assert (= n (read-sequence buffer stream)))
       (octets-to-string buffer)))))

(defun find-symbol-in-all-packages (name)
  (let (symbols)
    (do-all-symbols (symbol symbols)
      (when (string-equal symbol name)
        (pushnew symbol symbols)))))

(defun find-symbol-interactively (package-name symbol-name usage)
  (let ((intern (or (string-equal package-name "KEYWORD")
                    (null usage))))
    (restart-case
        (multiple-value-bind (symbol status)
            (funcall (if intern
                         #'intern
                         #'find-symbol)
                     symbol-name
                     (or (find-package package-name)
                         (error "package ~A for symbol ~A~@[ naming ~A~] not found" package-name symbol-name usage)))
          (if (or intern status)
              symbol
              (error "symbol ~A~@[ naming ~A~] not found in package ~A" symbol-name usage package-name)))
      (use-other-symbol (new-symbol)
        :interactive (lambda ()
                       (format t "Enter symbol~@[ (homonyms: ~{~S~^, ~})~]: " (find-symbol-in-all-packages symbol-name))
                       (let ((new-symbol (ignore-errors (read))))
                         (list new-symbol)))
        :report (lambda (stream) (format stream "Use another symbol~@[, homonyms: ~S~]" (find-symbol-in-all-packages symbol-name)))
        new-symbol)
      (read-as-nil ()
        :report "Read symbol as NIL"
        nil))))

(defun %decode-symbol (stream &key (intern t) usage)
  (let ((package-name (%decode-string stream))
        (symbol-name (%decode-string stream)))
    (when intern
      (find-symbol-interactively package-name symbol-name usage))))

(defun %decode-list (stream)
  (let* ((n (%decode-integer stream))
         (result (loop repeat n collect (decode stream)))
         (tail (and (plusp n) (decode stream))))
    (when tail
      (setf (cdr (last result)) tail))
    result))

(defun %decode-hash-table (stream)
  (let* ((test (%decode-symbol stream :usage "hash table test"))
         (rehash-size (%decode-double-float stream))
         (n (%decode-integer stream))
         (result (make-hash-table :test test :size n :rehash-size rehash-size)))
    (dotimes (x n)
      (let ((key (decode stream))
            (value (decode stream)))
        (setf (gethash key result) value)))
    result))

(defun %decode-single-float (stream)
  #+allegro
  (excl::shorts-to-single-float (%decode-uint16 stream)
                                (%decode-uint16 stream))
  #+cmu
  (kernel:make-single-float (%decode-sint32 stream))
  #+openmcl
  (make-single-float (%decode-sint32 stream))
  #+sbcl
  (sb-kernel:make-single-float (%decode-sint32 stream))
  #+lispworks
  (float-features:bits-single-float (%decode-sint32 stream)))

(defun %decode-double-float (stream)
  #+allegro
  (excl::shorts-to-double-float (%decode-uint16 stream)
                                (%decode-uint16 stream)
                                (%decode-uint16 stream)
                                (%decode-uint16 stream))
  #+cmu
  (kernel:make-double-float (%decode-sint32 stream)
                            (%decode-uint32 stream))
  #+openmcl
  (make-double-float (%decode-sint32 stream)
                     (%decode-uint32 stream))
  #+sbcl
  (sb-kernel:make-double-float (%decode-sint32 stream)
                               (%decode-uint32 stream))
  #+lispworks
  (let ((hi (%decode-uint32 stream))
        (lo (%decode-uint32 stream)))
    (float-features:bits-double-float (logior (ash hi 32) lo))))

(defun %decode-array (stream)
  (let* ((element-type
           (let ((type (%decode-symbol stream :usage "array element type")))
             (cond
               ((eql 'new-list-type type)
                (%decode-list stream))
               (t type))))
         (flags (read-byte stream))
         (vectorp (logbitp 0 flags))
         (adjustablep (logbitp 1 flags))
         (fill-pointer-p (logbitp 2 flags))
         (dimensions
          (if vectorp
              (list (%decode-integer stream))
              (loop repeat (%decode-integer stream)
                 collect (%decode-integer stream))))
         (fill-pointer
          (if fill-pointer-p
              (%decode-integer stream)
              nil))
         (result (make-array dimensions
                             :element-type element-type
                             :adjustable adjustablep
                             :fill-pointer fill-pointer)))
    (dotimes (i (reduce #'* dimensions))
      (setf (row-major-aref result i) (decode stream)))
    result))

(defun decode (stream)
  (let ((tag (%read-tag stream)))
    (case tag
      (#\a (%decode-array stream))
      (#\i (%decode-integer stream))
      (#\y (%decode-symbol stream))
      (#\c (%decode-char stream))
      (#\s (%decode-string stream))
      (#\l (%decode-list stream))
      (#\# (%decode-hash-table stream))
      (#\f (%decode-single-float stream))
      (#\d (%decode-double-float stream))
      (#\r (%decode-rational stream))
      (t (decode-object tag stream)))))

(defgeneric decode-object (tag stream))

;;;; OpenMCL does not have these functions
(defun make-single-float (bits)
  (cond
    ;; IEEE float special cases
    ((zerop bits) 0.0)
    ((= bits #x-80000000) -0.0)
    (t (let* ((sign (ecase (ldb (byte 1 31) bits)
                      (0  1.0)
                      (1 -1.0)))
              (iexpt (ldb (byte 8 23) bits))
              (expt (if (zerop iexpt)   ; denormalized
                        -126
                        (- iexpt 127)))
              (mant (* (logior (ldb (byte 23 0) bits)
                               (if (zerop iexpt)
                                   0
                                   (ash 1 23)))
                       (expt 0.5 23))))
         (* sign (expt 2.0 expt) mant)))))

#+openmcl
(defun make-double-float (hi lo)
  (cond
    ;; IEEE float special cases
    ((and (zerop hi) (zerop lo)) 0.0d0)
    ((and (= hi #x-80000000) (zerop lo)) -0.0d0)
    (t (let* ((bits (logior (ash hi 32) lo))
              (sign (ecase (ldb (byte 1 63) bits)
                      (0  1.0d0)
                      (1 -1.0d0)))
              (iexpt (ldb (byte 11 52) bits))
              (expt (if (zerop iexpt)   ; denormalized
                        -1022
                        (- iexpt 1023)))
              (mant (* (logior (ldb (byte 52 0) bits)
                               (if (zerop iexpt)
                                   0
                                   (ash 1 52)))
                       (expt 0.5d0 52))))
         (* sign (expt 2.0d0 expt) mant)))))
