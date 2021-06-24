;;;
;;;
;;; Copyright (c) 2009,2017, Jean-Claude Beaudoin
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

;;#-(or sbcl cmu clisp mkcl ecl ccl scl)
;;(error "Sorry, CL+J 0.4 does not support this Lisp.")

(defpackage #:cl+j-system
  (:use #:cl #:asdf))
(in-package #:cl+j-system)

(defsystem cl+j
  :description "CL+J: A Common Lisp + Java Interface."
  :version "0.4"
  :author "Jean-Claude Beaudoin <jean.claude.beaudoin@gmail.com>"
  :licence "MIT style license, Copyright (c) Jean-Claude Beaudoin 2009,2017."
  :components ((:file "cl+j_pkg")
	       (:file "reference" :depends-on ("cl+j_pkg"))
	       (:file "vtable" :depends-on ("reference"))
	       (:file "jni" :depends-on ("vtable"))
	       (:file "cl+j" :depends-on ("jni"))
	       (:file "java_callback" :depends-on ("cl+j"))
	       (:file "java_adapter" :depends-on ("java_callback"))
	       #+sbcl (:file "sbcl_repl")
	       )
  :depends-on (cffi trivial-garbage bordeaux-threads)
  )
