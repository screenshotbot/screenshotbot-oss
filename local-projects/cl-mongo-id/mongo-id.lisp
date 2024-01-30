;;; This is a library for generating MongoDB ObjectIDs per the client
;;; specification:
;;;
;;;   https://github.com/mongodb/specifications/blob/master/source/objectid.rst
;;;
;;; Full documentation on github:
;;;
;;;   https://github.com/orthecreedence/cl-mongo-id
;;;
;;; Please enjoy irresponsibly =].
;;;
;;; Andrew Lyon <orthecreedence@gmail.com>

(defpackage :cl-mongo-id
  (:use :cl)
  (:import-from #:cl-intbytes
                #:int64->octets)
  (:export :oid
           :oid-str
           :get-timestamp
           :get-hostname
           :get-pid
           :get-inc
           :reset-state)
  (:nicknames :mongoid))
(in-package :cl-mongo-id)

(defvar *id-inc*)
(defvar *id-inc-lock* (bt:make-lock))

;; In previous version of the ObjectId spec this was referenced as the
;; machine and pid. In v0.2 of the spec if was made to be a random
;; 5-byte sequence randomly chosen per instance of the process.
(defvar *random-value*)

(defun get-current-pid (&key if-not-exists-return)
  "Get the current process' PID. This function does it's best to be cross-
   implementation. If it isn't able to grab the PID from the system, it defaults
   to returning whatever value is passed into the :if-not-exists-return key."
  #+clisp
  (system::process-id)
  #+(and lispworks unix)
  (system::getpid)
  #+(and sbcl unix)
  (sb-unix:unix-getpid)
  #+(and cmu unix)
  (unix:unix-getpid)
  #+openmcl
  (ccl::getpid)
  #+ecl
  (ext:getpid)
  #-(or clisp (and lispworks unix) (and sbcl unix) (and cmu unix) (and openmcl unix) openmcl ecl)
  if-not-exists-return)

(defun reset-state ()
  "Resets (or sets) the random state associated with the current
process. This is useful when cl-mongo-id is loaded into an image that
is deployed to multiple machines. In that case you should call
RESET-STATE at the start of the process.

On SBCL, CCL and Lispworks, this is done automatically when a saved
image is restored.

The spec is okay with using non-cryptographic random numbers. But in
that case we're expected to seed the RNG with something that depends
on timestamp, pid, and hostname. The in-build RNG in CL doesn't give
us a way to explicitly seed an RNG, so instead we generate the random
numbers and XOR it with the hash. (The XOR operation will keep the
final output random, and will also make it unlikely that two servers
will end up generating the random numbers.)"
  (let ((state (make-random-state t))
        (hash (sxhash (format nil "~a.~a.~a"
                              (local-time:now)
                              (get-current-pid)
                              (uiop:hostname)))))
    (flet ((safe-random (number)
             (mod
              (logxor (random number)
                      hash)
              number)))
      (setf *id-inc* (safe-random (ash 1 24)))
      (let* ((value (safe-random (ash 1 40)))
             (octets (int64->octets value)))
        (let ()
          (setf *random-value*
                (subseq octets 0 5))))))
  ;; For easy debugging, we return the state (since we need to
  ;; visually check if the outputs look random enough. Unit tests can
  ;; be tricky to check).
  (values
   (cl-intbytes:int32->octets *id-inc*)
   *random-value*))

(reset-state)

#+sbcl
(pushnew 'reset-state sb-ext:*init-hooks*)

#+lispworks
(lw:define-action "When starting image" "Reset cl-mongo-id state"
  #'reset-state)

#+ccl
(pushnew 'reset-state ccl:*restore-lisp-functions*)

(defun oid (&optional id)
  "Generate a mongo id, in byte vector format."
  (cond
    ((stringp id) (convert-hex-vector id))
    ((vectorp id) id)
    ((null id)    (create-new-id))))

(defun oid-str (oid)
  "Given a vector ID, convert it to a string."
  (let ((hex-string (make-string 24)))
    (loop for byte across oid
          for i from 0 by 2 do
      (let ((byte-hex (format nil "~2,'0X" byte)))
        (setf (aref hex-string i) (aref byte-hex 0)
              (aref hex-string (1+ i)) (aref byte-hex 1))))
    hex-string))

(defun get-timestamp (oid &key bytes)
  "Grab the timestamp out of a vector oid. Passing :bytes t will return an array
  of bytes corresponding to the timestamp part of the ID instead of parsing it
  as an integer."
  (let ((timestamp (subseq oid 0 4)))
    (if bytes
        timestamp
        (convert-vector-int timestamp))))

(defun get-hostname (oid &key bytes)
  (error "get-hostname is no longer supported by the ObjectId spec"))

(defun get-pid (oid &key bytes)
  (error "get-pid is no longer supported by the ObjectId spec"))

(defun get-inc (oid &key bytes)
  "Grab the inc value out of a vector oid. Passing :bytes t will return an array
  of bytes corresponding to the inc value of the ID instead of parsing it as an
  integer."
  (let ((inc (subseq oid 9)))
    (if bytes
        inc
        (convert-vector-int inc))))

(defun convert-hex-vector (hex-string)
  "Takes a hex string, IE 4f2b8096 and converts it into a byte array:
      4f2b8096 -> #(79 43 128 150)
  Hex string *must* have even number of bytes."
  (assert (evenp (length hex-string)))
  (let* ((octet-count (floor (length hex-string) 2))
         (vector (make-array octet-count :element-type '(unsigned-byte 8))))
    (loop for i below octet-count
          for j from 0 by 2
          for k from 2 by 2 do
      (setf (aref vector i)
            (parse-integer hex-string :start j :end k :radix 16)))
    vector))

(defun convert-vector-int (vector)
  "Convert a byte array to an integer:
      #(79 150 243 81) -> 1335292753"
  (reduce (lambda (x y)
            (+ (ash x 8) y))
          vector))

(defun create-new-id ()
  "Create a brand-spankin-new ObjectId using the current timestamp/inc values,
  along with hostname and process pid."
  (let ((timestamp (logand #xFFFFFFFF (get-current-timestamp)))
        (inc (get-inc-val)))
    (let ((timestamp-bytes (convert-hex-vector (format nil "~8,'0X" timestamp)))
          (inc-bytes (convert-hex-vector (format nil "~6,'0X" inc))))
      (concatenate 'vector timestamp-bytes *random-value* inc-bytes))))

(defun get-current-timestamp ()
  "Get current unix timestamp."
  (local-time:timestamp-to-unix (local-time:now)))

(defun get-inc-val ()
  "Thread-safe method to get current ObjectId inc value. Takes an optional
  timestamp value to calculate inc for."
  (bt:with-lock-held (*id-inc-lock*)
    (setf *id-inc* (logand #xFFFFFF (1+ *id-inc*)))
    *id-inc*))
