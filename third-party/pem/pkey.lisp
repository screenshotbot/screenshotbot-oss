(defpackage #:pem/pkey
  (:use #:cl)
  (:import-from #:pem/parser
                #:parse-file)
  (:import-from #:asn1
                #:decode
                #:rsa-public-key-info)
  (:import-from #:cl-base64
                #:base64-string-to-usb8-array)
  (:import-from #:ironclad)
  (:export #:read-from-file))
(in-package #:pem/pkey)

(defun read-public-key (key)
  (let* ((der (base64:base64-string-to-usb8-array key))
         (der (asn1:decode der)))
    (optima:match der
      ((asn1:rsa-public-key-info n e)
       (ironclad:make-public-key :rsa :n n :e e))
      (otherwise (error "Unexpected format: ~S" key)))))

(defun read-private-key (key)
  (let* ((der (base64:base64-string-to-usb8-array key))
         (der (asn1:decode der)))
    (optima:match der
      ((asn1:rsa-private-key :private-exponent d :modulus n)
       (ironclad:make-private-key :rsa :d d :n n))
      (otherwise (error "Unexpected format: ~S" key)))))

(defun read-from-file (pem)
  (let ((data (pem/parser:parse-file pem)))
    (let ((public-key (cdr (assoc "PUBLIC KEY" data :test #'string=)))
          (private-key (cdr (assoc "RSA PRIVATE KEY" data :test #'string=))))
      (cond
        (public-key (read-public-key public-key))
        (private-key (read-private-key private-key))))))
