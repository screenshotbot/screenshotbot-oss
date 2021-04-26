(defpackage #:encrypt
  (:use #:cl)
  (:export :encrypt
           :decrypt))

(in-package #:encrypt)

(defvar *aes-key* (coerce #(5 35 203 1 188 14 179 215 174 44 58 47 118 95 88 247) '(vector (unsigned-byte 8))))
(defvar *iv* (coerce #(198 109 200 13 230 47 240 50 14 161 76 86 149 126 21 127) '(vector (unsigned-byte 8))))
(defvar *zero-byte* (coerce 0 '(unsigned-byte 8)))

(defun get-cipher ()
  (ironclad:make-cipher 'ironclad:aes :key *aes-key* :mode 'ironclad:cbc :initialization-vector *iv*))

(defun coerce-to-byte-array (a)
  (coerce a '(vector (unsigned-byte 8))))

(defun add-padding (a)
  (let ((l (length a)))
    (let ((padding (make-array (- 16 (mod l 16)) :initial-element *zero-byte*)))
      (coerce-to-byte-array (concatenate 'vector a padding)))))

(defun remove-padding (a)
  (coerce-to-byte-array (apply 'vector
                 (loop for c across a
                    if (not (eq *zero-byte* c))
                    collect c))))

(defun encrypt (a)
  (let ((in-array (ironclad:ascii-string-to-byte-array a)))
    (let ((in-array (add-padding in-array)))
      (ironclad:encrypt-in-place (get-cipher) in-array)
      (ironclad:byte-array-to-hex-string in-array))))

(defun decrypt (a)
  (let ((in-array (ironclad:hex-string-to-byte-array a)))
    (ironclad:decrypt-in-place (get-cipher) in-array)
    (map 'string #'code-char (remove-padding in-array))))
