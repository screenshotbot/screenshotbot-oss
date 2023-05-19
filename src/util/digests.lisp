;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/digests
  (:use #:cl)
  (:import-from #:util/health-check
                #:def-health-check)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:sha256-file
   #:md5-file))
(in-package :util/digests)

(fli:define-c-struct evp-md-ctx)
(fli:define-c-struct evp-md)

(defun crypto-module ()
  "On Mac, it appears there's a separate comm::crypto-library. Not sure
if there's a situation where we need to reference that module
directly."
  'comm::openssl-library)

(fli:define-foreign-function (evp-md-ctx-new "EVP_MD_CTX_new")
    ()
  :module (crypto-module)
  :result-type (:pointer evp-md-ctx))


(fli:define-foreign-function (evp-md-ctx-free "EVP_MD_CTX_free")
    ((ctx (:pointer evp-md-ctx)))
  :module (crypto-module)
  :result-type :void)

(def-easy-macro with-evp-md-ctx (&binding ctx &fn fn)
  (let ((ctx (evp-md-ctx-new)))
    (unwind-protect
         (funcall fn ctx)
      (evp-md-ctx-free ctx))))

(fli:define-foreign-function (evp-get-digestbyname "EVP_get_digestbyname")
    ((name (:reference :ef-mb-string)))
  :module (crypto-module)
  :result-type (:pointer evp-md))

(fli:define-foreign-function (digest-init "EVP_DigestInit_ex")
    ((ctx (:pointer evp-md-ctx))
     (evp-md (:pointer evp-md))
     (engine :pointer))
  :module (crypto-module)
  :result-type :int)

(fli:define-foreign-function (digest-update "EVP_DigestUpdate")
    ((ctx (:pointer evp-md-ctx))
     (data :lisp-simple-1d-array)
     (len :size-t))
  :module (crypto-module)
  :result-type :int)

(fli:define-foreign-function (digest-final "EVP_DigestFinal_ex")
    ((ctx (:pointer evp-md-ctx))
     (md :lisp-simple-1d-array)
     (size (:reference-return (:unsigned :int))))
  :module (crypto-module)
  :result-type :int)

(defun digest-file (file &key digest-name
                           digest-length #| EVP_MD_get_size is hard to port. See variations in OpenSSL 1.1, 3.0, and LibreSSL. |#)
  (comm:ensure-ssl)
  (let ((evp-md (evp-get-digestbyname digest-name)))
    (with-evp-md-ctx (ctx)
      (digest-init ctx evp-md nil)
      (let* ((buf-size (* 16 1024))
             (buffer (make-array buf-size :element-type '(unsigned-byte 8)
                                          :allocation :pinnable))
             (result (make-array digest-length :element-type '(unsigned-byte 8)
                                               :initial-element 0
                                               :allocation :pinnable)))
        (with-open-file (stream file :direction :input
                                     :element-type '(unsigned-byte 8))
          (loop named inner
                for bytes = (read-sequence buffer stream)
                while t
                do
                   (if (= bytes 0)
                       (return-from inner))
                   (unless (= 1
                              (digest-update ctx
                                             buffer
                                             bytes))
                     (error "Could not update digest"))))
        (multiple-value-bind (result size)
            (digest-final ctx result nil)
          (unless (= 1 result)
            (error "Could not finalize SHA digest"))
          (unless (eql digest-length size)
            (error "Digest length mismatch, this could mean corruption in memory")))
        result))))


(defun sha256-file (file)
  (digest-file file
               :digest-name "sha256"
               :digest-length 32))

(defun md5-file (file)
  (digest-file file
               :digest-name "md5"
               :digest-length 16))

(def-health-check verify-libcrypto-digests ()
  (uiop:with-temporary-file (:pathname p :stream s)
    (write-string "foobar" s)
    (finish-output s)
    (flet ((ensure (expected actual)
             (unless (equalp expected actual)
               (error "Expected digest of ~a, got ~a"
                      expected actual))))
      (ensure #(195 171 143 241 55 32 232 173 144 71 221 57 70
                107 60 137 116 229 146 194 250 56 61 74 57 96
                113 76 174 240 196 242)
        (sha256-file p))
      (ensure #(56 88 246 34 48 172 60 145 95 48 12 102 67 18 198 63)
        (md5-file p)))))
