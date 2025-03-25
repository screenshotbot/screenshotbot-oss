;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :encrypt/hmac
  (:use #:cl)
  (:import-from #:encrypt
                #:key-from-disk)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:util/misc
                #:or-setf)
  (:export
   #:sign-hmac
   #:verify-hmac))
(in-package :encrypt/hmac)

(defvar *hmac-key* nil)

(defun hmac-key ()
  (or-setf
   *hmac-key*
   (key-from-disk "sha256-hmac-key.bin" 64)
   :thread-safe t))

(defmethod sign-hmac (bytes)
  (let ((mac (ironclad:make-mac :hmac
                                (hmac-key)
                                :sha256)))
    (ironclad:update-mac mac bytes)
    (ironclad:produce-mac mac)))

(defmethod sign-hmac ((str string))
  (sign-hmac (flex:string-to-octets str)))

(defmethod verify-hmac (input hmac)
  (let ((sign (sign-hmac input)))
    (equalp sign hmac)))


(defmethod encode-signed-string ((str string))
  "Returns a string that is signed and timestamped"
  (let* ((ts (get-universal-time))
         (sign (sign-hmac (format nil "~a.~a" str ts))))
    (json:encode-json-to-string
     `((:input . ,str)
       (:ts . ,ts)
       (:sign . ,(ironclad:byte-array-to-hex-string sign))))))

(define-condition invalid-signature-error (error)
  ())

(define-condition invalid-signature-timestamp (error)
  ())

(defmethod decode-signed-string ((str string))
  (let ((obj (json:decode-json-from-string str)))
    (let ((ts (get-universal-time)))
      (unless (< (abs (- ts (assoc-value obj :ts))) 300)
        (error 'invalid-signature-timestamp)))
    (unless (verify-hmac (format nil "~a.~a" (assoc-value obj :input)
                                 (assoc-value obj :ts))
                         (ironclad:hex-string-to-byte-array
                          (assoc-value obj :sign)))
      (error 'invalid-signature-error))
    (assoc-value obj :input)))
