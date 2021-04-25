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

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find (find-package :cl+j) (package-use-list *package*))
    (use-package :cl+j)))

(declaim (optimize safety debug))
;(declaim (optimize (debug 0) (speed 3)))

(java-import "EndOfReplException")
(java-import "ReplFromJava")
(java-import "LispRef")

(defun quit-java-repl ()
  (throw-java-exception "EndOfReplException" (jstring "Goodbye!"))
  )


(def-java-native
    "ReplFromJava"
  ("int" "read" () ()
	 (new-lisp-reference (read)))
  ("int" "eval" (("int" ref)) ()
	 ;;(break "In eval native.")
	 (new-lisp-reference (eval (lisp-reference-value ref))))
  ("int" "print" (("int" ref)) ()
	 ;;(break "In print native.")
	 (new-lisp-reference 
	  (prog1 (print (lisp-reference-value ref))
	    (finish-output))))
  ("void" "prompt" (("String" prompt)) ()
	  ;;(break "In prompt native.")
	  (format t "~%~A" (jtol prompt))
	  (finish-output)
	  )
  )


(load "java_lisp_ref")

(java-init)

(#]ReplFromJava.repl)
