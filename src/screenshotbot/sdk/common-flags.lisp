;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/common-flags
  (:use #:cl
        #:com.google.flag)
  (:shadow #:define-flag)
  (:export
   #:*verbose*
   #:*help*
   #:*sdk-flags*
   #:define-flag
   #:obsolete?))
(in-package :screenshotbot/sdk/common-flags)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *sdk-flags* (make-hash-table)))

;; For easy reloading, this can be deleted in the future, but might
;; require a restart of the server.
#+lispworks
'(*hostname*
 *api-key*
 *api-secret*)

(defmacro define-flag (name &rest args
     &key selector &allow-other-keys)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ,(intern (str:upcase selector) "KEYWORD") *sdk-flags*)
           ',name)
     (com.google.flag:define-flag ,name
       ,@args)))

(define-flag *verbose*
  :default-value nil
  :selector "verbose"
  :type boolean
  :help "Verbose logs")

(define-flag *help*
  :selector "help"
  :default-value nil
  :type boolean)

(defun obsolete? (flag)
  (str:containsp "[OBSOLETE]" (com.google.flag::help flag)))
