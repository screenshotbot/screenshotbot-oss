;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/digests
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:sha256-file))
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
  #+nil ;; we don't want a moment of no module exist
  (fli:disconnect-module :util-digest-native)
  (fli:register-module
   :util-digest-native
   :real-name output))

(fli:define-foreign-function (sizeof-sha256-ctx "tdrhq_sizeof_SHA256_CTX")
    ()
  :result-type :int)

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

(defun sha256-file (file)
  (comm:ensure-ssl)
  (fli:with-dynamic-foreign-objects ((sha-ctx :char :nelems (+ 20 (sizeof-sha256-ctx))))
    (unless (= 1 (sha256-init sha-ctx))
      (error "could not init SHA digest"))
    (let* ((buf-size (* 16 1024))
           (digest-length 32)
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
                            (sha256-update sha-ctx
                                           buffer
                                           bytes))
                   (error "Could not update digest"))))
      (unless (= 1 (sha256-final result sha-ctx))
        (error "Could not finalize SHA digest"))
      result)))
