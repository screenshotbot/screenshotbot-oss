;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/copy-file
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:copy-file-fast)
  #+sbcl
  (:local-nicknames (#:fli #:util/fake-fli)))
(in-package :util/copy-file)

(fli:define-foreign-function link
    ((src (:reference-pass :ef-mb-string))
     (dest (:reference-pass :ef-mb-string)))
  :result-type :int)

(defparameter +exdev+ 18
  "A hard link to another file system... this constant is correct at
 least on Linux and Darwin.")

(defun copy-file-fast (src dest)
  "Copies the file from src to dest, if it's possible to use a hard link
 then it will do so."
  (let ((ret (link (namestring src) (namestring dest))))
    (unless (= ret 0)
      (let* ((errno #+lispworks (lw:errno-value)
                    #-lispworks (osicat-posix:get-errno))
             (message #+lispworks
                      (lw:get-unix-error errno)
                      #-lispworks
                      errno))
        (cond
          ((= errno +exdev+)
           (warn "Could not create hard link, falling back to manual copy, reason: ~a"
                 message)
           (uiop:copy-file src dest))
          (t
           (error "Could not create hard link: ~a" message)))))))
