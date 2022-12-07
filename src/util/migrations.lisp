;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/migrations
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:ensure-symbol-in-package
   #:clone-slot))
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

(defun find-symbol* (name package)
  (when (find-package package)
    (find-symbol name package)))

(defun call-ensure-symbol-in-package (name old new)
  (let ((old-sym (find-symbol* (string name) old))
        (new-sym (find-symbol* (string name) new)))
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

(defmacro clone-slot (class &key from to)
  ;; This code will do nothing if run from a bootup
  ;; (i.e. before bknr.datastore has loaded). So it must be run as
  ;; part of a live migration.
  `(call-clone-slot ',class
                    :from ',from
                    :to ',to))

(defun call-clone-slot (class &key from to)
  (dolist (inst (bknr.datastore:class-instances class))
    (cond
      ((and
        (not (slot-boundp inst to))
        (slot-boundp inst from))
       (with-transaction ()
         (setf (slot-value inst to)
               (slot-value inst from)))))))
