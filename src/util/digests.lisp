;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/digests
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:sha256-file
   #:md5-file))
(in-package :util/digests)

#|
# define SHA_LBLOCK      16

typedef struct SHA512state_st {
    SHA_LONG64 h[8];
    SHA_LONG64 Nl, Nh;
    union {
        SHA_LONG64 d[SHA_LBLOCK];
        unsigned char p[SHA512_CBLOCK];
    } u;
    unsigned int num, md_len;
} SHA512_CTX;
|#

(let ((output (asdf:output-file 'asdf:compile-op
                                 (asdf:find-component :util/digests
                                                      "digest"))))
  (fli:register-module
   :util-digest-native
   :real-name output))

(fli:define-foreign-function (sizeof-sha256-ctx "tdrhq_sizeof_SHA256_CTX")
    ()
  :result-type :int)

(fli:define-foreign-function (sizeof-md5-ctx "tdrhq_sizeof_MD5_CTX")
    ()
  :result-type :int)

(defparameter *sizeof-sha256-ctx* (sizeof-sha256-ctx))
(defparameter *sizeof-md5-ctx* (sizeof-md5-ctx))

(fli:disconnect-module :util-digest-native)

(fli:define-foreign-function (sha256-init "SHA256_Init")
    ((ctx (:pointer :void)))
  :result-type :int)

(fli:define-foreign-function (sha256-update "SHA256_Update")
    ((ctx (:pointer :void))
     (data :lisp-simple-1d-array)
     (len :size-t))
  :result-type :int)

(fli:define-foreign-function (sha256-final "SHA256_Final")
    ((md :lisp-simple-1d-array)
     (ctx (:pointer :void)))
  :result-type :int)


(fli:define-foreign-function (md5-init "MD5_Init")
    ((ctx (:pointer :void)))
  :result-type :int)

(fli:define-foreign-function (md5-update "MD5_Update")
    ((ctx (:pointer :void))
     (data :lisp-simple-1d-array)
     (len :size-t))
  :result-type :int)

(fli:define-foreign-function (md5-final "MD5_Final")
    ((md :lisp-simple-1d-array)
     (ctx (:pointer :void)))
  :result-type :int)

(defun digest-file (file &key ctx-size
                           init
                           update
                           final
                           digest-length)
  (comm:ensure-ssl)
  (fli:with-dynamic-foreign-objects ((sha-ctx :char :nelems (+ 20 ctx-size)))
    (unless (= 1 (funcall init sha-ctx))
      (error "could not init SHA digest"))
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
                            (funcall update sha-ctx
                                           buffer
                                           bytes))
                   (error "Could not update digest"))))
      (unless (= 1 (funcall final result sha-ctx))
        (error "Could not finalize SHA digest"))
      result)))

(defun sha256-file (file)
  (digest-file file :ctx-size *sizeof-sha256-ctx*
                    :init #'sha256-init
                    :update #'sha256-update
                    :final #'sha256-final
                    :digest-length 32))

(defun md5-file (file)
  (digest-file file :ctx-size *sizeof-md5-ctx*
                    :init #'md5-init
                    :update #'md5-update
                    :final #'md5-final
                    :digest-length 16))
