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


;; You have to adjust the Java classpath according to your need.
(setq *jvm-options* 
      (append *jvm-options* 
	      '("-Djava.class.path=.:/home/Jean-Claude/Jabberwocky/Jabberwocky.jar:/home/Jean-Claude/Projects/cl+j/cl_j.jar")))

(java-init) ;; must not have been called before 

(defvar *jabber-main-args* nil)

(setq *jabber-main-args* (jnew[] "String" '(2)))

(defvar arg0)
(setq arg0 (jstring "-i"))

(defvar arg1)
(setq arg1 (jstring  "/home/Jean-Claude/Jabberwocky"))

(setf (jaref *jabber-main-args* 0) arg0)
(setf (jaref *jabber-main-args* 1) arg1)

(jmethod "IDE.Main" "main" *jabber-main-args*)

;;;
;;; Beware that Jabberwocky calls System.exit() when it quits.
;;;   That is quite a show stopper!
;;;