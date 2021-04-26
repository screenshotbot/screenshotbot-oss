;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :util)

(defvar *package-to-acceptors* nil)

(defun %register-acceptor (acceptor-name package)
  (let ((package (find-package package)))
    (assert (symbolp acceptor-name))
    (setf (alexandria:assoc-value *package-to-acceptors* package)
          acceptor-name)))

(defmacro register-acceptor (acceptor-name package)
  `(eval-when (:compile-top-level :execute)
     (%register-acceptor ,acceptor-name ,package)))

(defun package-acceptor-name (&optional (package *package*))
  (assoc-value *package-to-acceptors* (find-package package)))
