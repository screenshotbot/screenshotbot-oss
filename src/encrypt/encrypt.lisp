;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage #:encrypt
  (:use #:cl)
  (:import-from #:flexi-streams
                #:octets-to-string)
  (:export :encrypt
   :decrypt
           :encrypt-mongoid
           :decrypt-mongoid
           :encrypt-number
           :decrypt-number))

(in-package #:encrypt)

(defvar *lock* (bt:make-lock))

(defun key-from-disk (filename bytes)
  (base64:base64-string-to-usb8-array
   (bt:with-lock-held (*lock*)
     (let ((file (path:catfile
                  (cond
                    ((ignore-errors util/store:*object-store*)
                     (util/store:object-store))
                    (t
                     ;; just for test convenience
                     (bknr.datastore::store-directory bknr.datastore:*store*)))
                  filename)))
       (flet ((read-file () (str:trim
                             (uiop:read-file-string file))))
         (cond
           ((path:-e file)
            (read-file))
           (t
            (let ((string (base64:usb8-array-to-base64-string
                           (secure-random:bytes bytes secure-random:*generator*))))
              (with-open-file (stream file :direction :output)
                (write-string string stream))
              (read-file)))))))))

(defun blowfish-key ()
  (key-from-disk "blowfish-key.txt" 8))

(defvar *zero-byte* (coerce 0 '(unsigned-byte 8)))

(defvar *blowfish-cipher* nil)
(defun get-blowfish-cipher ()
  (util/misc:or-setf
   *blowfish-cipher*
   (ironclad:make-cipher 'ironclad:blowfish :mode :ecb :key (blowfish-key))
   :thread-safe t))

(defvar *aes-128-cipher* nil)

(defun get-aes-128-cipher ()
  (util/misc:or-setf
   *aes-128-cipher*
   (ironclad:make-cipher 'ironclad:aes
                          :mode :ecb :key (key-from-disk "aes-128-key.txt" 16))))

(defun coerce-to-byte-array (a)
  (coerce a '(vector (unsigned-byte 8))))

(defun add-padding (a)
  (let ((pad-length 8))
   (let ((l (length a)))
     (let ((padding (make-array  (mod (- pad-length (mod l pad-length)) pad-length) :initial-element *zero-byte*)))
       (coerce-to-byte-array (concatenate 'vector a padding))))))

(defun remove-padding (a)
  (coerce-to-byte-array (apply 'vector
                 (loop for c across a
                    if (not (eq *zero-byte* c))
                    collect c))))

(defun encrypt (a)
  (encrypt-array (ironclad:ascii-string-to-byte-array a)))


(defun encrypt-array (arr &optional (cipher (get-blowfish-cipher))
                            (base64-padding 0)
                            (version 0))
  (let* ((in-array (add-padding arr))
         (output (make-array (+
                              (length in-array)
                              base64-padding) :element-type '(unsigned-byte 8)
                                               :initial-element 0)))
    (multiple-value-bind (consumed produced)
        (ironclad:encrypt cipher in-array output :padding 0 :handle-final-block t)
      (assert (= consumed (length in-array)))
      (assert (= produced (length in-array))))

    (when (> base64-padding 0)
     (setf (elt output (1- (length output))) version))

    (base64:usb8-array-to-base64-string output :uri t)))

(defun decrypt (a)
  (let ((in-array (base64:base64-string-to-usb8-array a :uri t)))
    (ironclad:decrypt-in-place (get-blowfish-cipher) in-array)
    (map 'string #'code-char (remove-padding in-array))))

(defun decrypt-array (arr length &optional (cipher (get-blowfish-cipher)))
  "Decrypt an encoded array. However, the length of the array is not
  known, so you must provide that as an argument."
  (let ((in-array (base64:base64-string-to-usb8-array arr :uri t)))
    (ironclad:decrypt-in-place cipher in-array)
    (subseq in-array 0 length)))

(defconstant +mongoid-length+ 12)

(defconstant +num-max-bytes+ 32)

(defun encrypt-mongoid (mongoid)
  "For encrypting mongoids, we use AES-128. Using a 64-bit blowfish key
 can reveal some information that can be used to recreate mongoids,
 even though the likelyhood is really low. Basically the first 8 bytes
 are timestamp and process id, and the last 8 bytes has only about
 2^24 different values, (assuming the process id remains the same). So
 if you know a timestamp, you can try 16m different second-halfs to
 retrieve another valid mongoid within the same timestamp. You could
 probably be more clever about limiting the search set, or by forcing
 generations of mongoids to get back to a previously known number. But
 again, the likelyhood is very low.

 Encrypting the 12 bytes with a 16-byte cipher will result in a 16-byte
 output. Base64 encoding this will require 4/3*18 = 24 bytes. This
 means we have two extra bytes to store an unencryption version
 information of the encoding. If we see padding characters `..`
 instead we assuming it's the v0 blowfish encryption.
 "
  (when (stringp mongoid)
    (error "Please provide the mongoid as an array"))
  (unless (= +mongoid-length+ (length mongoid))
    (error "Invalid mongoid length"))
  (encrypt-array mongoid (get-aes-128-cipher) 2 1))

(defun decrypt-mongoid (encrypted-mongoid)
  (cond
    ((and
      (= 24 (length encrypted-mongoid))
      (eql #\. (elt encrypted-mongoid 23)))
     (decrypt-array  encrypted-mongoid +mongoid-length+
                    (get-blowfish-cipher)))
    (t
     (decrypt-array encrypted-mongoid +mongoid-length+
                    (get-aes-128-cipher)))))


(defun encrypt-number (x)
  (assert (< x (ash 1 (* 8 +num-max-bytes+))))
  (format nil "encn_~a"
          (encrypt-array (cl-intbytes:int->octets x +num-max-bytes+)
                         (get-aes-128-cipher)
                         +num-max-bytes+)))

(defun decrypt-number (encrypted-number)
  (assert (str:starts-with-p "encn_" encrypted-number))
  (let ((base64 (str:substring 5 nil encrypted-number)))
    (cl-intbytes:octets->int
     (decrypt-array base64 +num-max-bytes+
                    (get-aes-128-cipher))
     +num-max-bytes+)))
