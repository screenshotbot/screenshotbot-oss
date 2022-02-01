;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :util)

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

(defun funcall-if (fn arg &rest rest)
  "If arg is not nil, then call the function fn with the args (cons arg rest)"
  (when arg
    (apply fn arg rest)))

(defmacro ?. (fn arg &rest rest)
  (alexandria:with-gensyms (arg-val)
    `(let ((,arg-val ,arg))
       (funcall-if (function ,fn)
                   ,arg-val
                   ,@rest))))

(local-time:reread-timezone-repository)

(defmacro or-setf (place expr)
  `(or
    ,place
    (setf ,place ,expr)))
