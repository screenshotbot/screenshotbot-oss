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

(java-init) ;; if not already done.



(in-java-context "hello-swt")

(java-import "org.eclipse.swt.*")
(java-import "org.eclipse.swt.widgets.*")


#|
;; Version 1.  Event loop and application model creation all rolled into one function.
(defun hello-swt ()
  (with-java-context "hello-swt"
    (let (swt-display shell hello-text)
      (setq swt-display (jnew "Display"))
      (setq shell (jnew "Shell" swt-display))
      (setq hello-text (jnew "Text" shell #?SWT.CENTER))

      (#_setText hello-text (jstr "Hello SWT!"))
      (#_pack hello-text)
      (#_pack shell)
      (#_open shell)

      (format t "~%Entering the application event loop.~%")
      (do ()
	  ((jtrue-p (#_isDisposed shell)))
	(unless (jtrue-p (#_readAndDispatch swt-display))
	  ;;(format t "About to sleep.~%")
	  (#_sleep swt-display))
	)
      (format t "About to dispose of the whole thing!~%")
      (#_dispose swt-display)
      )
    )
  )

(defun say-hello-swt ()
  (bt:make-thread #'hello-swt :name "CL+J demos: Hello SWT!"))

(say-hello-swt)
|#


#|
;; Version 2. Event loop and application model creation in seperate yet explicit functions.
(defvar *swt-display* nil)

(let (#|swt-display|# end-swt-event-loop)

  (defun hello-swt ()
    (with-java-context "hello-swt"
      (let (shell hello-text)
	(unless *swt-display*
	  (setq *swt-display* (jnew "Display")))
	(setq shell (jnew "Shell" *swt-display*))
	(setq hello-text (jnew "Text" shell #?SWT.CENTER))

	(#_setText hello-text (jstr "Hello SWT!"))
	(#_pack hello-text)
	(#_pack shell)
	(#_open shell)

	)
      )
    )

  (defun swt-event-loop ()
    (format t "~%Entering the application event loop.~%")
    (do ()
	(end-swt-event-loop)
      (unless (jtrue-p (#_readAndDispatch *swt-display*))
	(#_sleep *swt-display*))
      )
    (format t "About to dispose of the whole thing!~%")
    (#_dispose *swt-display*)
    (setq *swt-display* nil end-swt-event-loop nil)
    )

  (defun stop-swt-event-loop ()
    (setq end-swt-event-loop t)
    (#_wake *swt-display*)
    )

  )


(defun say-hello-swt ()
  (if *swt-display*
      (#_asyncExec *swt-display* (jnew "cl_j.RunInLisp" (new-lisp-reference #'hello-swt)))
      (format t "~&There is no SWT event loop!~%"))
  )

(defun start-to-say-hello-to-swt ()
  (bt:make-thread #'(lambda () (hello-swt) (swt-event-loop))
		  :name 
		  ;;"CL+J demos: Hello SWT!" 
		  '(:name "CL+J demos: Hello SWT!" :initial-bindings nil)
		  ))

(start-to-say-hello-to-swt)
|#

;; Version 3. Event loop in a CL+J package. Only application model defined in a function here.
(defun hello-swt ()
  (with-java-context "hello-swt"
    (let (shell hello-text)
      (setq shell (jnew "Shell" (swt:display)))
      (setq hello-text (jnew "Text" shell #?SWT.CENTER))

      (#_setText shell (jstr "CL+J Demos: Hello SWT!"))
      (#_setText hello-text (jstr "Hello SWT!"))
      (#_pack hello-text)
      (#_pack shell)
      (#_open shell)
      )
    )
  )

(swt:start-swt-event-loop)

(swt::wait-on-swt-event-loop-startup 10)

(swt:In-UI-thread-do #'hello-swt)



