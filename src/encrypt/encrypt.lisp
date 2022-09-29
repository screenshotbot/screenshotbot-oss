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
           :decrypt-mongoid))

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

(defun coerce-to-byte-array (a)
  (coerce a '(vector (unsigned-byte 8))))

(defun add-padding (a)
  (let ((pad-length 8))
   (let ((l (length a)))
     (let ((padding (make-array (- pad-length (mod l pad-length)) :initial-element *zero-byte*)))
       (coerce-to-byte-array (concatenate 'vector a padding))))))

(defun remove-padding (a)
  (coerce-to-byte-array (apply 'vector
                 (loop for c across a
                    if (not (eq *zero-byte* c))
                    collect c))))

(defun encrypt (a)
  (encrypt-array (ironclad:ascii-string-to-byte-array a)))

(defun encrypt-array (arr)
  (let* ((in-array (add-padding arr))
         (output (make-array (length in-array) :element-type '(unsigned-byte 8)
                                               :initial-element 0)))
    (multiple-value-bind (consumed produced)
        (ironclad:encrypt (get-blowfish-cipher) in-array output :padding 0 :handle-final-block t)
      (assert (= consumed (length in-array)))
      (assert (= produced (length output))))
    (base64:usb8-array-to-base64-string output :uri t)))

(defun decrypt (a)
  (let ((in-array (base64:base64-string-to-usb8-array a :uri t)))
    (ironclad:decrypt-in-place (get-blowfish-cipher) in-array)
    (map 'string #'code-char (remove-padding in-array))))

(defun decrypt-array (arr length)
  "Decrypt an encoded array. However, the length of the array is not
  known, so you must provide that as an argument."
  (let ((in-array (base64:base64-string-to-usb8-array arr :uri t)))
    (ironclad:decrypt-in-place (get-blowfish-cipher) in-array)
    (subseq in-array 0 length)))

(defconstant +mongoid-length+ 12)

(defun encrypt-mongoid (mongoid)
  (when (stringp mongoid)
    (error "Please provide the mongoid as an array"))
  (unless (= +mongoid-length+ (length mongoid))
    (error "Invalid mongoid length"))
  (encrypt-array mongoid))

(defun decrypt-mongoid (encrypted-mongoid)
  (decrypt-array encrypted-mongoid +mongoid-length+))
