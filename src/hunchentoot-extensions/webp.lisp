;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :hunchentoot-extensions/webp
  (:use #:cl
        #:hex)
  (:local-nicknames (#:a #:alexandria)))
(in-package :hunchentoot-extensions/webp)

(defun supports-webp? ()
  (if (boundp 'hunchentoot:*request*)
      (str:contains?  "image/webp" (hunchentoot:header-in :accept hunchentoot:*request*))))

(defun fix-for-webp (url &key force)
  (cond
    ((or force (supports-webp?))
     (let ((parts (str:split "?" url)))
       (str:join "?" (cons
                      (ppcre:regex-replace "\\.[a-z]*$" (car parts) ".webp")
                      (cdr parts)))))
    (t
     url)))
