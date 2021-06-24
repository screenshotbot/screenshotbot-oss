;;;
;;;
;;; Copyright (c) 2009, Jean-Claude Beaudoin
;;; All rights reserved by the author.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;;
;;;

(defpackage #:cl+j-boot
  (:use #:cl #:asdf))
(in-package #:cl+j-boot)


#|
;;; this is very SBCLish.
#+sbcl
(eval-when (:execute :compile-toplevel :load-toplevel)
  (require 'cffi))
#+clisp
(eval-when (:execute :compile-toplevel :load-toplevel)
  (asdf:oos 'asdf:load-op :cffi))
|#

(defun cffi-version ()
  (asdf:component-version (asdf:find-system 'cffi)))

(defpackage jni
  (:use "CL" "CFFI")
  (:export create-java-vm
	   jstring
	   ;;
	   new-lisp-reference
	   free-lisp-reference
	   lisp-reference-value
	   lisp-reference-eq
	   new-lref
	   free-lref
	   lref-val
	   lref-eq
	   ))

(defpackage cl+j
  (:use "CL" "JNI" "CFFI")
  (:export java-init
	   java-destroy
	   jfield ;; ?? not really needed.
	   jmethod ;; ?? not really needed
	   jeq
	   jtrue
	   jtrue-p
	   jfalse
	   jfalse-p
	   jnull
	   jinstanceof
	   jref
	   weak-jref
	   new-local-ref
	   with-jvm-local-frame
	   jtol
	   add-jtol-mapping
	   delete-jtol-mapping
	   ltoj
	   jprim-boolean
	   jprim-char
	   jprim-byte
	   jprim-short
	   jprim-int
	   jprim-long
	   jprim-float
	   jprim-double
	   jstr
	   ;;jstring
	   jstring-length
	   jnew
	   jnew[]
	   jaref
	   jlength
	   *java-context*
	   in-java-context
	   make-java-context
	   delete-java-context
	   find-java-context
	   with-java-context
	   find-java-class
	   java-import
	   throw-java-exception
	   handle-java-exception ;; needed?
	   def-java-native
	   ;;register-pending-java-natives ;; needed?
	   ;;
	   new-lisp-reference ;; re-export
	   free-lisp-reference ;; re-export
	   lisp-reference-value ;; re-export
	   lisp-reference-eq  ;; re-export
	   new-lref
	   free-lref
	   lref-val
	   lref-eq
	   ))

(in-package cl+j)

(export 'jstring)


#|

(defun build ()
  (compile-file "vtable.lisp")
  (load "vtable" :verbose t)
  (compile-file "jni.lisp")
  (load "jni" :verbose t)
  (compile-file "cl+j.lisp")
  (load "cl+j" :verbose t)
  )

;(eval-when (:load-toplevel)
;  (build))

|#

(delete-package '#:cl+j-boot)
