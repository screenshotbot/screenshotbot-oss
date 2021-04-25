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

(in-package :jni)

;(declaim (optimize debug))
;(declaim (optimize (debug 0) (speed 3)))


(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *debug* t))

(defmacro debug-progn (&body body)
  (if *debug*
      `(progn ,@body)
      nil))

;;(defparameter *jni-trace* t) ;#+cmu19 t #+(or sbcl clisp) nil)
(defparameter *jni-trace* #+scl t
	                  #+cmu19 nil
			  #+ccl nil
			  #+clisp t
			  #+(or sbcl mkcl ecl) nil)
(defparameter *jni-describe-exception* nil)


;;;
;;;
;;;


;(eval-when (#| :execute :load-toplevel |# :compile-toplevel)
  (defun build-vtable-dispatch-vector (slots)
    (mapcar
     #'(lambda (slot)
	 (cond ((symbolp slot)
		(list slot :pointer)
		)
	       ((not (listp slot))
		(error "Invalid slot definition.")
		)
	       (t
		(let ((slot-name (car slot)))
		  (list slot-name :pointer))
		)
	       )
	 )
     slots)
    )

  (defun nflatten-1 (al)
    (do ((p al))
	((null p) al)
      (let ((q (car p)))
	(cond ((null q)
	       (cond ((null (cdr p))
		      (setq al (butlast al))
		      (pop p))
		     (t
		      (rplaca p (cadr p))
		      (rplacd p (cddr p))
		      ))
	       )
	      ((listp q)
	       (nconc q (cdr p))
	       (rplacd p (cdr q))
	       (rplaca p (car q))
	       (pop p)
	       )
	      (t 
	       (pop p)
	       )
	      )
	)))


;;  We define our condition on "error"
;;  but we should handle on "serious-condition" when going back to Java.

(declaim (ftype (function (condition stream) t) print-thrown-from-java))
;;(declaim (ftype (function (condition stream) t) print-java-throwable))

(define-condition thrown-from-java (condition)
  ((this :initarg :this :reader thrown-from-java-this))
  (:report print-thrown-from-java)
  )

(export '(thrown-from-java thrown-from-java-this))

;; (define-condition java-throwable (condition)
;;   ((thrown :initarg :thrown
;; 	   :reader java-throwable-thrown))
;;   (:report print-java-throwable)
;;   )

;; (export '(java-throwable java-throwable-thrown))

(defun delete-thrown-java-global-ref (global-java-ref)
#|
  (debug-progn 
    #-sbcl (format t "~%Inside delete-thrown-java-global-ref!~%") 
    #+sbcl (format t "~%Inside delete-thrown-java-global-ref! Thread = ~S.~%"
		   (sb-thread:thread-name sb-thread:*current-thread*))
    (finish-output))
|#

  (handler-case 
      (unless (java-vm-destroyed)
	(DeleteGlobalRef global-java-ref))
    (jni::jni-error (condition)
      (declare (ignorable condition))
#|
      (format t "~%In delete-thrown-java-global-ref: condition = ~S~%" condition)
      (apply #'format t (simple-condition-format-control condition)
	     (simple-condition-format-arguments condition))
      (fresh-line)
      (finish-output)
      ;;(break)
|#
      ;;(debug-progn (signal condition))
      )
    )
  )

(defun wrap-throwable (lref-to-throwable)
  (let* ((gref (NewGlobalRef lref-to-throwable))
	 (wrapper (make-condition 'thrown-from-java :this gref)
	  ))
    (DeleteLocalRef lref-to-throwable)
    #-sbcl (tg:finalize wrapper #'(lambda () (delete-thrown-java-global-ref gref)))
    wrapper
    )
  )


;;;
;;;
;;;

(defun build-error-handling-block-for-void (throws)
  (declare (ignore throws))
  )

(defun build-error-handling-block (ret-c-type throws ok not-ok error)
  (declare (ignore ret-c-type throws ok not-ok error))
  )


(defun jfield-int (object name)
  (PushLocalFrame 4)
  (let* ((class (GetObjectClass object))
	 (field-id (GetFieldID class name "I"))
	 )
    (prog1
	(GetIntField object field-id)
      (PopLocalFrame (null-pointer)))
    )
  )


(defparameter +LispCondition-class+ nil)
;; +LispCondition-class+ is to be bound to a global ref to the Java wrapper class.

(defun find-class-LispCondition ()
  (setq +LispCondition-class+ 
	(let ((lref (FindClass "cl_j/LispCondition")))
	  (prog1 (NewGlobalRef lref) (DeleteLocalRef lref)))))

(defmacro LispCondition-class ()
  `(or +LispCondition-class+ (find-class-LispCondition)))

(defmacro relay-lisp-condition-in-transit (j-excep)
  `(let* ((throwable-gref (thrown-from-java-this ,j-excep))
	  (j-excep-class (GetObjectClass throwable-gref)) ;; This one generates a lref!
	  (is-same (IsSameObject j-excep-class (LispCondition-class)))
	  )
     (DeleteLocalRef j-excep-class) ;; dispose of j-excep-class lref.
     (when (eql JNI_TRUE is-same)
       ;;(break "Received a Lisp Condition in a Java Wrapper.")
       (let* ((condi-ref (jfield-int throwable-gref "condition"))
	      (condi (lisp-reference-value condi-ref))
	      )
	 ;;(break "About to re-signal a condition.")
	 (error condi)
	 (error "!! Came back from re-signalling a Lisp condition transmitted by Java! !!")
	 )
       )
     )
  )
;;
;;
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cffi-version ()
    (asdf:component-version (asdf:find-system 'cffi)))
  (defun cffi-ver->=-0.11.0 ()
    (if (uiop:version< (cffi-version) "0.11.0") '#.(gensym) :common))
  )


;;; This macro was required by a backward incompatible change
;;; that occurred in CFFI around Feb 14, 2007 according to its ChangeLog.
;;; Version 0.9.2 pre-dates the change. We hope the other versions
;;; we use to post-date it.
(defmacro my-foreign-funcall (name-or-pointer &rest args)
  (if (uiop:version<= "0.9.2" (cffi-version))
      `(foreign-funcall ,name-or-pointer ,@args)
      `(foreign-funcall-pointer ,name-or-pointer () ,@args))
  )



(defun build-vtable-function-wrappers (vname this-accessor slots export-all)
  ;;(declare (ignore export-all))
  ;;(format t "Inside build-vtable-function-wrappers.~%")
  ;;(format t " Value of slots: ~S~%" slots)
  ;;(force-output)
  (let (defuns)
    (dolist (slot slots (nreverse defuns))
      (unless (symbolp slot) ;; just skip over placeholders.
	(let (defun f-funcall-sig args throws-p)
	  (unless (and (listp slot) (<= 3 (length slot)))
	    ;;(break)
	    (error "Invalid vtable slot specification. ~S" slot))
	  (destructuring-bind (slot-name 
			       ret-c-type 
			       ( &rest c-args )
			       &key 
			       (throws nil throws-specific-p)
			       (throws-any-exception nil)
			       (ok nil ok-p) 
			       (not-ok nil not-ok-p)
			       (error nil error-p)
			       (export nil export-p))
	      slot
	    (declare (ignorable throws error) (ignorable export-p))
	    ;;(declare (ignore #|throws ok error|# export))
	    ;;(format t "c-args = ~S~%" c-args)
	    (debug-progn
	      (when *jni-trace*
		(format t "In ~S~%" slot-name)
		(format t "  :throws ~S~%" throws)
		(if throws-any-exception
		    (format t "  :throws-any-exception ~S~%" throws-any-exception))
		(format t "  :ok ~S,  :not-ok ~S~%" ok not-ok)
		(format t "  :error ~S~%" error)))
	    (setq throws-p (or throws-specific-p throws-any-exception))
	    (dolist (c-arg c-args)
	      ;; a c-arg must be a list of 2 elements
	      (unless (null c-arg) ;; if it is empty we ignore it.
		(push (car c-arg) args)	;; collect arg names
		(push (reverse c-arg) f-funcall-sig))
	      )
	    (setq args (nreverse args))
	    (setq f-funcall-sig (nflatten-1 (nreverse f-funcall-sig)))
	    (setq 
	     defun
	     `(defun ,slot-name ,args
		(let ((func-ptr 
		       (foreign-slot-value (mem-ref ,this-accessor :pointer)
					   ',vname ',slot-name)))
		  ,(if (eq :void ret-c-type)
		       `(progn
			  (my-foreign-funcall func-ptr
					      :pointer ,this-accessor
					      ,@f-funcall-sig
					      ,ret-c-type)
			  ,@(when throws-p
				  `((when (= JNI_TRUE (ExceptionCheck))
				      (let ((j-excep 
					     (wrap-throwable
					      (prog1
						  (ExceptionOccurred)
						(debug-progn
						 (when *jni-describe-exception*
						   (ExceptionDescribe)))
						(ExceptionClear)
						))))
					,(when throws-any-exception
					       `(relay-lisp-condition-in-transit j-excep))
;; 					(error
;; 					 'thrown-from-java
;; 					 :thrown j-excep)
					(error j-excep)
					)

				      ;; Belt and suspenders.
				      (debug-progn
					(when *jni-describe-exception*
					  (ExceptionDescribe)))
				      (error "~S failed." ',slot-name)))
				  )
			  (debug-progn
			    (when *jni-trace*
			      (format t "~S.~%" ',slot-name)))
			  nil
			  )
		       `(let ((result (my-foreign-funcall func-ptr 
							  :pointer ,this-accessor
							  ,@f-funcall-sig 
							  ,ret-c-type)))
			  ,@(when 
			     ok-p
			     (cond
			       (error-p
				;; This one should be a lot more
				;; elaborate. Will do for now.
				;; It is meant to implement support for the :error
				;; vtable entry option. So don't use :error for now...
				`((debug-progn
				    (if *jni-trace*
					(break "Keyword :error in jni:defvtable is not implemented yet!")))
				  )
				)
			       (t
				`((unless (eql ,ok result)
				    ,@(when throws-p
					    `((let ((j-excep
						     (wrap-throwable
						      (prog1
							  (ExceptionOccurred)
							(debug-progn
							 (when *jni-describe-exception*
							   (ExceptionDescribe)))
							(ExceptionClear)
							))))
						,(when throws-any-exception
						       `(relay-lisp-condition-in-transit
							 j-excep))
;; 						(error
;; 						 'thrown-from-java
;; 						 :thrown j-excep)
						(error j-excep)
						)
					      ;; Belt and suspenders.
					      (debug-progn
						(when *jni-describe-exception*
						  (ExceptionDescribe)))
					      (error "~S failed." ',slot-name)))
				    ,@(when (not (or throws-p error-p))
					    `((error "~S failed. result = ~S.~%"
						     ',slot-name result)))))
				)
			       )
			     )
			  ,@(when 
			     not-ok-p
			     (cond
			       ((eq not-ok 'JNI_NULL)
				`((when (null-pointer-p result)
				    ,@(when throws-p
					    `((let ((j-excep
						     (wrap-throwable
						      (prog1
							  (ExceptionOccurred)
							(debug-progn
							 (when *jni-describe-exception*
							   (ExceptionDescribe)))
							(ExceptionClear)
							))))
						,(when throws-any-exception
						       `(relay-lisp-condition-in-transit
							 j-excep))
;; 						(error
;; 						 'thrown-from-java
;; 						 :thrown j-excep)
						(error j-excep)
						)
					      ;; Belt and suspenders.
					      (debug-progn
						(when *jni-describe-exception*
						  (ExceptionDescribe)))))
				    (error "~S failed. result = ~S.~%"
					   ',slot-name result)
				    ))
				)
			       (t
				`((when (eql ,not-ok result)
				    ,@(when throws-p
					    `((let ((j-excep
						     (wrap-throwable
						      (prog1
							  (ExceptionOccurred)
							(debug-progn
							 (when *jni-describe-exception*
							   (ExceptionDescribe)))
							(ExceptionClear)
							))))
						,(when throws-any-exception
						       `(relay-lisp-condition-in-transit
							 j-excep))
;; 						(error
;; 						 'thrown-from-java
;; 						 :thrown j-excep)
						(error j-excep)
						)
					      ;; Belt and suspenders.
					      (debug-progn
						(when *jni-describe-exception*
						  (ExceptionDescribe)))))
				    (error "~S failed. result = ~S.~%"
					   ',slot-name result)))
				)
			       )
			     )
			  ,@(when (and throws-p (not ok-p) (not not-ok-p))
				  `((when (= JNI_TRUE (ExceptionCheck))
				      (let ((j-excep 
					     (wrap-throwable
					      (prog1
						  (ExceptionOccurred)
						(debug-progn
						 (when *jni-describe-exception*
						   (ExceptionDescribe)))
						(ExceptionClear)
						))))
					,(when throws-any-exception
					       `(relay-lisp-condition-in-transit
						 j-excep))
;; 					(error
;; 					 'thrown-from-java
;; 					 :thrown j-excep)
					(error j-excep)
					)
				      ;; Belt and suspenders.
				      (debug-progn
					(when *jni-describe-exception*
					  (ExceptionDescribe)))
				      (error "~S failed. result = ~S~%"
					     ',slot-name result)))
				  )
;;; 			   ,@(WHEN (and throws-p (not ok-p) (not not-ok-p))
;;; 				   `((case (ExceptionCheck)
;;; 				       (JNI_TRUE
;;; 					(debug-progn
;;;                                       (when *jni-describe-exception* 
;;;                                         (ExceptionDescribe)))
;;; 					(error "~S failed. result = ~S~%"
;;; 					       ',slot-name result))
;;; 				       (JNI_FALSE)
;;; 				       (otherwise
;;; 					(error "JNI:ExceptionCheck failed!~%"))
;;; 				       )
;;; 				     )
;;; 				   )
			  (debug-progn
			    (when *jni-trace*
			      (format t "~S: result = ~S.~%" ',slot-name  result)))
			  result
			  )))))
	    ;;(format t "Defined slot function: ~S~%" defun)
	    (push defun defuns)
	    (when (or export-all export) (push `(export ',slot-name) defuns))
	    )
	  )
	)
      )
    )
  )
;  ) ;; eval-when

(defmacro defvtable (name this (&key export-all) &rest slots)
  `(progn
    (defcstruct ,name ,@(build-vtable-dispatch-vector slots))
    ,@(if (uiop:version< (cffi-version) "0.11.0")
          (build-vtable-function-wrappers name this slots export-all)
          (build-vtable-function-wrappers `(:struct ,name) this slots export-all)))
  )
