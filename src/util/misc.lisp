;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/misc
  (:use #:cl)
  (:export
   #:supports-webp?
   #:fix-for-webp
   #:funcall-if
   #:?.
   #:or-setf
   #:not-null!
   #:uniq))
(in-package :util/misc)


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

(defvar *lock* (bt:make-lock "or-setf"))

(defmacro or-setf (place expr &key (thread-safe nil)
                                (lock '*lock*))
  (cond
    (thread-safe
     `(or
       ,place
       (bt:with-lock-held (,lock)
         ,place
         (setf ,place ,expr))))
    (t
     `(or
       ,place
       (setf ,place ,expr)))))


(defun %not-null (x expr)
  (cond
    (x x)
    (t (error "Expected ~s to eval to a not null value" expr))))

(defmacro not-null! (x)
  `(%not-null ,x ',x))

(defun %not-null-or-empty (x expr)
  (cond
    ((not (str:emptyp  x)) x)
    (t (error "Expected ~s to eval to a non-empty value" expr))))

(defmacro not-empty! (x)
  `(%not-null-or-empty ,x ',x))

(defun uniq (list &key (test #'eql))
  "Given a sorted list, remove adjacent duplicates"
  (let ((ret nil))
    (loop for x in list
          if (or (null ret)
                 (not (funcall test x (car ret))))
            do (push x ret))
    (nreverse ret)))
