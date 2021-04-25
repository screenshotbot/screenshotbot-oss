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


(defpackage "SWT"
  (:use "CL" "CL+J")
  (:export make-listener
	   display
	   swt-init
	   start-swt-event-loop
	   stop-event-loop
	   In-UI-thread-do
	   In-UI-thread-do-and-wait
	   In-UI-thread-do-delayed
	   ))

(in-package :swt)

(defun make-listener (handler &key data)
  (let ((listener (jnew "cl_j.swt.LispListener" (new-lref handler) (new-lref data))))
    listener)
  )

(def-java-native
    "cl_j.swt.LispListener"
  ("void" "funcall" (("int" fun-ref) ("org.eclipse.swt.widgets.Event" event) ("int" data)) ()
	 (funcall (lref-val fun-ref) event data))
;;   ("void" "funcall" (("int" fun-ref)) ()
;; 	 (funcall (lref-val fun-ref) nil nil))
  ("void" "freeLispReference" (("int" ref)) ()
	  (free-lref ref))
  )



(defvar *display* nil)

(defvar *end-event-loop* nil)

(defvar *event-loop-thread* nil)

(defun display () *display*)

(defun event-loop ()
  (format t "~%Entering SWT's UI application event loop.~%") ;; debug
  (do ()
      (*end-event-loop*)
    (unless (jtrue-p (#]readAndDispatch *display*))
      (#]sleep *display*))
    )
  (format t "About to dispose of SWT's event loop!~%") ;; debug
  (setq *end-event-loop* nil)
 )

(defun stop-event-loop ()
  (setq *end-event-loop* t)
  (#]wake *display*)
  )

(defun swt-init ()
  (unless (and (boundp *display*) *display*)
    (setq *display* (jnew "org.eclipse.swt.widgets.Display"))))

(defun start-swt-event-loop ()
  (unless *event-loop-thread* 
    (setq *event-loop-thread*
	  (bt:make-thread
	   #'(lambda () 
	       #+mkcl (mp:thread-detach mp:*thread*)
	       (unwind-protect 
		    (progn
		      (swt-init)
		      (event-loop))
		 ;;(format t "~&Cleaning up on SWT event loop shutdown! *display* = ~S.~%" *display*) (finish-output) ;; debug JCB
		 (when *display* (#]dispose *display*))
		 (setq *display* nil *end-event-loop* nil)
 		 (setq *event-loop-thread* nil)
		 )
	       )
	   :name 
	   "SWT event loop" 
	   ;;'(:name "SWT event loop" :initial-bindings nil) ;; mkcl specific
	   ))))

(defun wait-on-swt-event-loop-startup (max_seconds)
  (do ((waited 0 (+ waited 0.1)))
      ((or (>= waited max_seconds) (and *event-loop-thread* *display*))) (sleep 0.1)))


(defun In-UI-thread-do (fun)
  (if *display*
      (#]asyncExec *display* (jnew "cl_j.RunInLisp" (new-lref fun)))
      (format t "~&There is no SWT event loop!~%"))
  )


(defun In-UI-thread-do-and-wait (fun)
  (if *display*
      (#]syncExec *display* (jnew "cl_j.RunInLisp" (new-lref fun)))
      (format t "~&There is no SWT event loop!~%"))
  )

(defun In-UI-thread-do-delayed (milliseconds fun)
  (if *display*
      (#]timerExec *display* milliseconds (jnew "cl_j.RunInLisp" (new-lref fun)))
      (format t "~&There is no SWT event loop!~%"))
  )


