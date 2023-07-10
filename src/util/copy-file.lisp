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
  #-lispworks
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
  #+windows
  (uiop:copy-file src dest)
  #-windows
  (let ((ret (link (namestring src) (namestring dest))))
    (unless (= ret 0)
      (let* ((errno
               (util/posix:errno))
             (message (util/posix:get-errno-string errno)))
        (cond
          ((= errno +exdev+)
           (log:warn "Could not hard link from ~a to ~a" src dest)
           (warn "Could not create hard link, falling back to manual copy, reason: ~a"
                 message)
           (uiop:copy-file src dest))
          (t
           (error "Could not create hard link: ~a" message)))))))
