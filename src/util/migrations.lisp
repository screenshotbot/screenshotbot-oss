;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/migrations
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/migrations)

(defvar *moved-syms* nil
  "A store of moved symbols, in case we need to recover something bad.")

(defmacro ensure-symbol-in-package (name &key old new)
  "Ensure the given symbol named by NAME is in the NEW package and not in
 the OLD package. If it's only in OLD it moves it. If it's only in NEW,
 it does
 nothing. If it does not exist at all, it creates it in NEW.

  It the same symbols exists in both packages, we only keep the NEW package reference.

   If a different symbol by that name exists in both packages, then an
 error is signaled.
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (call-ensure-symbol-in-package ',name ',old ',new)))

(define-condition symbol-in-both-packages (error)
  ())

(defun call-ensure-symbol-in-package (name old new)
  (let ((old-sym (find-symbol (string name) old))
        (new-sym (find-symbol (string name) new)))
    (pushnew (list old-sym new-sym)
             *moved-syms*
             :test #'equal)
    (cond
      ((and
        (not old-sym)
        (not new-sym))
       (let ((sym (intern (string name) new)))
         sym))
      ((and
        old-sym
        (not new-sym))
       (unintern old-sym old)
       (import old-sym new)
       old-sym)
      ((and
        (not old-sym)
        new-sym)
       (assert (eql
                (symbol-package new-sym)
                (find-package new)))
       new-sym)
      ((eql old-sym new-sym)
       (unintern old-sym old)
       (intern (string name) new)
       new-sym)
      (t
       (error 'symbol-in-both-packages)))))
