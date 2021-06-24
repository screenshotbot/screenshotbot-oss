;;;
;;;
;;; Copyright (c) 2009,2017,  Jean-Claude Beaudoin
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


(in-package :cl+j)

;(declaim (optimize debug))
;(declaim (optimize (debug 0) (speed 3)))


(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *debug* t))

(defmacro debug-progn (&body body)
  (if *debug*
      `(progn ,@body)
      nil))


(defmacro str+ (str1 &rest strs)
  `(concatenate 'simple-string ,str1 ,@strs)
  )

(defmacro empty-str-p (str)
  `(eql 0 (array-total-size ,str)))


;;;
;;;  Some basic predicates and constants.
;;;

(defconstant jtrue JNI_TRUE)
(defconstant jfalse JNI_FALSE)

(defun jtrue-p (val)
  (= JNI_TRUE val))

(defun jfalse-p (val)
  (= JNI_FALSE val))

(defun jobjectp (arg)
  (when (jref-p arg) (setq arg (jref-it arg))) ;; unbox it first.
  (cffi::pointerp arg))

(defparameter jnull (cffi::null-pointer))

(defun jnull (arg)
  (when (jref-p arg) (setq arg (jref-it arg))) ;; unbox it first.
  (and (cffi::pointerp arg) (cffi::null-pointer-p arg)))

(defmacro with-pinned-values ((&body vars) &body body)
  (let (pin-vars
	pin-bindings
	)
    (dolist (var vars)
      (let ((pin-var (gensym "pinned-")))
	(push pin-var pin-vars)
	(push (list pin-var var) pin-bindings)
	)
      )
    `(let ,pin-bindings
       (declare (special ,@pin-vars))
       ,@body
       )
    )
  )




;;;
;;; Some wrappers on Local/Global/WeakGlobal References.
;;;

;;; A weak reference should always be converted to a global or local reference
;;; before access due to the risk of asynchronous garbage collection in the JVM.
;;;

(defstruct weak-jref
  it
  )

(defun delete-java-weak-ref (weak-java-ref)
#|
  #-ecl (debug-progn (format t "~%Inside delete-java-weak-ref!~%") (finish-output))
|#
  (handler-case
      (unless (java-vm-destroyed)
	(DeleteWeakGlobalRef weak-java-ref))
    (jni::jni-error (condition)
      (declare (ignorable condition))
      ;;(debug-progn (signal condition))
      )
    )
  )

(defun weak-jref (ref)
  (let* ((java-weak-ref (NewWeakGlobalRef ref))
	 (obj (make-weak-jref :it java-weak-ref)))
    #-sbcl (tg:finalize obj #'(lambda () (delete-java-weak-ref java-weak-ref)))
    obj))


(defstruct jref
  it
  )

#+ecl (defparameter +delete-java-global-ref-has-run+ nil)
#+ecl (defvar *finalizer-process* nil)
#+ecl (defvar *deleted-ref* nil)

(defun delete-java-global-ref (global-java-ref)
  ;;(declare (ignore global-java-ref))
#|
  #-ecl (debug-progn
	  #-sbcl (format t "~%Inside delete-java-global-ref!~%")
	  #+sbcl (format t "~%Inside delete-java-global-ref! Thread = ~S.~%"
			 (sb-thread:thread-name sb-thread:*current-thread*))
	  (finish-output))
|#

  (handler-case
      (unless (java-vm-destroyed)
	#+ecl (setq *deleted-ref* global-java-ref)
	(DeleteGlobalRef global-java-ref))
    (jni::jni-error (condition)
      (declare (ignorable condition))
#|
      (format t "~%In delete-java-global-ref: condition = ~S~%" condition)
      (apply #'format t (simple-condition-format-control condition)
	     (simple-condition-format-arguments condition))
      (fresh-line)
      (finish-output)
      ;;(break)
|#
      ;;(debug-progn (signal condition))
      )
    )
  #+ecl (setq +delete-java-global-ref-has-run+ t)
  #+ecl (setq *finalizer-process* mp:*current-process*)
  )

(defun jref (ref)
  ;;(break "in jref")
  (when (weak-jref-p ref) (setq ref (weak-jref-it ref))) ;; unbox it.
  (let* ((global-java-ref (NewGlobalRef ref))
	 ;;(cons-global-java-ref (cons global-java-ref nil))
	 (obj (make-jref :it global-java-ref)))
    #-sbcl (tg:finalize obj #'(lambda () (delete-java-global-ref global-java-ref)))
    obj))

(defun as-jref-value (lref)
  (prog1
      (jref lref)
    (DeleteLocalRef lref)
    )
  )

(defun new-local-ref (ref)
  (cond ((weak-jref-p ref) (setq ref (weak-jref-it ref))) ;; unbox it.
	((jref-p ref) (setq ref (jref-it ref))) ;; unbox it.
	)
  (NewLocalRef ref)
  )

(defun delete-local-ref (lref)
  (DeleteLocalRef lref))


(defmacro with-jvm-local-frame ((&optional (capacity 16) &key local-ref-value) &body body)
  (if local-ref-value
      `(let (result)
	 (unwind-protect
	      (progn
		(PushLocalFrame ,capacity)
		(setq result (progn ,@body))
		)
	   (if (pointerp result)
	       (setq result (PopLocalFrame result))
	       (PopLocalFrame (null-pointer)))
	   )
	 result
	 )
      `(unwind-protect
	    (progn
	      (PushLocalFrame ,capacity)
	      ,@body)
	 (PopLocalFrame (null-pointer)))
      )
  )

;;;
;;;
;;;

(defun %jeq (jobj1 jobj2) (= JNI_TRUE (IsSameObject jobj1 jobj2)))

(defun jeq (jobj1 jobj2)
  (with-pinned-values (jobj1 jobj2)
    (when (jref-p jobj1) (setq jobj1 (jref-it jobj1)))
    (when (jref-p jobj2) (setq jobj2 (jref-it jobj2)))
    (= JNI_TRUE (IsSameObject jobj1 jobj2))
    )
  )

(defun jinstanceof (ref jclazz)
  (with-pinned-values (ref jclazz)
    (when (jref-p ref) (setq ref (jref-it ref)))
    (when (jref-p jclazz) (setq jclazz (jref-it jclazz)))
    (cond ((not (pointerp ref))
	   (error "First argument to jinstanceof must be a foreign reference")
	   )
;;; 	  ((not (pointerp jclazz)) ;; not really needed on SBCL, CFFI does the validation.
;;; 	   (error "Second argument to jinstanceof must be a foreign reference")
;;; 	   )
	  ((null-pointer-p jclazz)
	   (error "Second argument to jinstanceof cannot be a foreign null reference")
	   )
	  ((null-pointer-p ref) nil)
	  (t
	   (eql JNI_TRUE (IsInstanceOf ref jclazz))))))


;;;
;;; Some basic Java primitive JNI type wrapper.
;;;

(defstruct (jprim (:constructor make-jprim-1))
  type
  value
  )

#+(or)
(defun free-jprim (this) ;; Either we free manually with this one or we use a finalizer.
  (foreign-free (jprim-value this)))

(defun make-jprim (&key type value)
  (let ((it (make-jprim-1 :type type :value value)))
    #+(and) (tg:finalize it #'(lambda () (foreign-free (jprim-value it))))
    it))

(defun jprim-boolean (val)
  (make-jprim :type 'jboolean :value (foreign-alloc 'jboolean :initial-element val))
  )

(defun jprim-char (val)
  (make-jprim :type 'jchar :value (foreign-alloc 'jchar :initial-element val))
  )

(defun jprim-byte (val)
  (make-jprim :type 'jbyte :value (foreign-alloc 'jbyte :initial-element val))
  )

(defun jprim-short (val)
  (make-jprim :type 'jshort :value (foreign-alloc 'jshort :initial-element val))
  )

(defun jprim-int (val)
  (make-jprim :type 'jint :value (foreign-alloc 'jint :initial-element val))
  )

(defun jprim-long (val)
  (make-jprim :type 'jlong :value (foreign-alloc 'jlong :initial-element val))
  )

(defun jprim-float (val)
  (make-jprim :type 'jfloat :value (foreign-alloc 'jfloat :initial-element val))
  )

(defun jprim-double (val)
  (make-jprim :type 'jdouble :value (foreign-alloc 'jdouble :initial-element val))
  )


;;;
;;; Some basic primitives to support Java method calls
;;;

(defmacro descriptor-step (sig index length)
  `(if (< ,index ,length)
      (incf ,index)
      (error "Malformed Java descriptor: ~S~%" ,sig))
  )

(defmacro skip-object-spec (sig index length)
  `(do ()
       ((char= #\; (schar ,sig ,index)))
     (descriptor-step ,sig ,index ,length)
     )
  )

(defmacro skip-array-spec (sig index length)
  `(progn
     ;; First we skip the "["
     (do ()
	 ((char/= #\[ (schar ,sig ,index)))
       (descriptor-step ,sig ,index ,length)
       )
     ;; Second we swallow the component descriptor.
     (case (schar ,sig ,index)
       (#\L (skip-object-spec ,sig ,index ,length))
       ((#\I #\J #\Z #\C #\B #\S #\F #\D))
       (t (error "Malformed Java descriptor: ~S~%" ,sig))
       )
     )
  )

(defmacro fill-args (sig jargs &rest args)
  (let (body arg)
    `(let ((i 0)
	   (len (length ,sig))
	   )
       ,@(dotimes (rank (length args) (nreverse body))
	  (setq arg (nth rank args))
	  (push
	   `(let ((arg-val-buf
		   (if (jprim-p ,arg)
		       (mem-ref (jprim-value ,arg) (jprim-type ,arg))
		       ,arg)))
	      (case (schar ,sig (descriptor-step ,sig i len))
		(#\I (setf (foreign-slot-value
			    (mem-aptr! ,jargs (union-jvalue) ,rank) (union-jvalue) 'i) arg-val-buf))
		(#\L (setf (foreign-slot-value
			    (mem-aptr! ,jargs (union-jvalue) ,rank) (union-jvalue) 'l) arg-val-buf)
		     (skip-object-spec ,sig i len))
		(#\[ (setf (foreign-slot-value
			    (mem-aptr! ,jargs (union-jvalue) ,rank) (union-jvalue) 'l) arg-val-buf)
		     (skip-array-spec ,sig i len))
		(#\J (setf (foreign-slot-value
			    (mem-aptr! ,jargs (union-jvalue) ,rank) (union-jvalue) 'j) arg-val-buf))
		(#\Z (setf (foreign-slot-value
			    (mem-aptr! ,jargs (union-jvalue) ,rank) (union-jvalue) 'z) arg-val-buf))
		(#\C (setf (foreign-slot-value
			    (mem-aptr! ,jargs (union-jvalue) ,rank) (union-jvalue) 'c) arg-val-buf))
		(#\B (setf (foreign-slot-value
			    (mem-aptr! ,jargs (union-jvalue) ,rank) (union-jvalue) 'b) arg-val-buf))
		(#\S (setf (foreign-slot-value
			    (mem-aptr! ,jargs (union-jvalue) ,rank) (union-jvalue) 's) arg-val-buf))
		(#\F (setf (foreign-slot-value
			    (mem-aptr! ,jargs (union-jvalue) ,rank) (union-jvalue) 'f) arg-val-buf))
		(#\D (setf (foreign-slot-value
			    (mem-aptr! ,jargs (union-jvalue) ,rank) (union-jvalue) 'd) arg-val-buf))
		)
	      )
	   body)
	  )
       )
    )
  )

(defun dyn-fill-args (sig jargs args)
  (let ((i 0)
	(len (length sig)))
    (do ((arg (car args) (car args)) ;;times (rank (length args))
	 (rank 0 (1+ rank)))
        ((endp args))
      ;(break)
      (when (null arg) (setq arg jnull))
      (when (jref-p arg) (setq arg (jref-it arg))) ;; unbox it first.

      (when (jprim-p arg)
	(setq arg (mem-ref (jprim-value arg) (jprim-type arg))))
      (case (schar sig (descriptor-step sig i len))
	(#\I (setf (foreign-slot-value
		    (mem-aptr! jargs (union-jvalue) rank) (union-jvalue) 'i) arg))
	(#\L (setf (foreign-slot-value
		    (mem-aptr! jargs (union-jvalue) rank) (union-jvalue) 'l) arg)
	     (skip-object-spec sig i len))
	(#\[ (setf (foreign-slot-value
		    (mem-aptr! jargs (union-jvalue) rank) (union-jvalue) 'l) arg)
	     (skip-array-spec sig i len))
	(#\J (setf (foreign-slot-value
		    (mem-aptr! jargs (union-jvalue) rank) (union-jvalue) 'j) arg))
	(#\Z (setf (foreign-slot-value
		    (mem-aptr! jargs (union-jvalue) rank) (union-jvalue) 'z) arg))
	(#\C (setf (foreign-slot-value
		    (mem-aptr! jargs (union-jvalue) rank) (union-jvalue) 'c) arg))
	(#\B (setf (foreign-slot-value
		    (mem-aptr! jargs (union-jvalue) rank) (union-jvalue) 'b) arg))
	(#\S (setf (foreign-slot-value
		    (mem-aptr! jargs (union-jvalue) rank) (union-jvalue) 's) arg))
	(#\F (setf (foreign-slot-value
		    (mem-aptr! jargs (union-jvalue) rank) (union-jvalue) 'f) arg))
	(#\D (setf (foreign-slot-value
		    (mem-aptr! jargs (union-jvalue) rank) (union-jvalue) 'd) arg))
	(t (error "Unknown java descriptor value ~S.~%" (schar sig i)))
	)
      (pop args)
      )
    )
  )


(defun jmethod-void-1 (this method-name arg sig)
  (PushLocalFrame 4)
  (let* ((this-class (GetObjectClass this))
	 (method-id (GetMethodID this-class method-name sig))
	 )
    (with-foreign-object (args (union-jvalue) 2)
      (fill-args sig args arg)
      (CallVoidMethodA this method-id args)
      (PopLocalFrame (null-pointer))
      )
    )
  )

(defun jmethod-boolean-0 (this method-name)
  (PushLocalFrame 4)
  (let* ((this-class (GetObjectClass this))
	 (method-id (GetMethodID this-class method-name "()Z"))
	 (result (CallBooleanMethodA this method-id (null-pointer)))
	 )
    (PopLocalFrame (null-pointer))
    result
    )
  )

(defun jmethod-boolean-1 (this method-name arg sig)
  (PushLocalFrame 4)
  (let* ((this-class (GetObjectClass this))
	 (method-id (GetMethodID this-class method-name sig))
	 result
	 )
    (with-foreign-object (args (union-jvalue) 2)
      (fill-args sig args arg)
      (setq result (CallBooleanMethodA this method-id args))
      (PopLocalFrame (null-pointer))
      result
      )
    )
  )

(defun jmethod-static-boolean-1 (class method-name arg sig)
  (PushLocalFrame 4)
  (let* ((method-id (GetStaticMethodID class method-name sig))
	 result
	 )
    (with-foreign-object (args (union-jvalue) 2)
      (fill-args sig args arg)
      (setq result (CallStaticBooleanMethodA class method-id args))
      (PopLocalFrame (null-pointer))
      result


      )
    )
  )

(defun jmethod-int-0 (this method-name)
  (PushLocalFrame 4)
  (let* ((this-class (GetObjectClass this))
	 (method-id (GetMethodID this-class method-name "()I"))
	 (result (CallIntMethodA this method-id (null-pointer)))
	 )
    (PopLocalFrame (null-pointer))
    result
    )
  )

(defun jmethod-object-0 (this method-name sig)
  (PushLocalFrame 4)
  (let* ((this-class (GetObjectClass this))
	 (method-id (GetMethodID this-class method-name sig))
	 )
    (PopLocalFrame (CallObjectMethodA this method-id (null-pointer)))
    )
  )

(defun jmethod-object-1 (this method-name arg sig)
  (PushLocalFrame 4)
  (let* ((this-class (GetObjectClass this))
	 (method-id (GetMethodID this-class method-name sig))
	 )
    (with-foreign-object (args (union-jvalue) 2)
      (fill-args sig args arg)
      (PopLocalFrame (CallObjectMethodA this method-id args))
      )
    )
  )

(defun jmethod-object-2 (this method-name arg1 arg2 sig)
  (PushLocalFrame 4)
  (let* ((this-class (GetObjectClass this))
	 (method-id (GetMethodID this-class method-name sig))
	 )
    (with-foreign-object (args (union-jvalue) 3)
      (fill-args sig args arg1 arg2)
      (PopLocalFrame (CallObjectMethodA this method-id args))
      )
    )
  )

(defun jfield-static-int (class-designation name)
  (PushLocalFrame 4)
  (let* ((class (FindClass class-designation))
	 (field-id (GetStaticFieldID class name "I"))
	 result
	 )
    (setq result (GetStaticObjectField class field-id))
    (PopLocalFrame (null-pointer))
    result
    )
  )

(defun jfield-static-object (class-designation name sig)
  (PushLocalFrame 4)
  (let* ((class (FindClass class-designation))
	 (field-id (GetStaticFieldID class name sig))
	 )
    (PopLocalFrame (GetStaticObjectField class field-id))
    )
  )

(defun jfield-int (object name)
  (PushLocalFrame 4)
  (let* ((class (GetObjectClass object))
	 (field-id (GetFieldID class name "I"))
	 )
    (prog1
	(GetIntField object field-id)
      (PopLocalFrame (null-pointer))
      )
    )
  )

(defun jfield-object (object name sig)
  (PushLocalFrame 4)
  (let* ((class (GetObjectClass object))
	 (field-id (GetFieldID class name sig))
	 )
    (PopLocalFrame (GetObjectField object field-id))
    )
  )


;;;
;;;  Java Reflection caching support
;;;

(defvar java-String nil)
(defvar java-Class nil)
(defvar java-Modifier nil)


(defun init-java-basic-classes ()
  (setq java-String
	(find-java-class "java/lang/String"))
  (setq java-Class
	(find-java-class "java/lang/Class"))
  (setq java-Modifier
	(find-java-class "java/lang/reflect/Modifier"))
  )

(defvar java-NoClassDefFoundError nil)
(defvar java-ClassFormatError nil)
(defvar java-ClassCircularityError nil)
(defvar java-ExceptionInInitializerError nil)
(defvar java-OutOfMemoryError nil)


(defun init-java-exceptions ()
  (setq java-NoClassDefFoundError
	(find-java-class "java/lang/NoClassDefFoundError"))
  (setq java-ClassFormatError
	(find-java-class "java/lang/ClassFormatError"))
  (setq java-ClassCircularityError
	(find-java-class "java/lang/ClassCircularityError"))
  (setq java-ExceptionInInitializerError
	(find-java-class "java/lang/ExceptionInInitializerError"))
  (setq java-OutOfMemoryError
	(find-java-class "java/lang/OutOfMemoryError"))
  )

(defmacro handle-java-exception (form &rest handlers)
  `(handler-case ,form
     (thrown-from-java (condi)
       ;(ExceptionClear)
       (let* ((j-exception (thrown-from-java-this condi))
	      (j-exception-class (GetObjectClass j-exception))) ;; lref
	 (cond
	   ,@(let (code)
		  (dolist (handler handlers (nreverse code))
		    (destructuring-bind (target (&optional arg) &rest body) handler
		      (let* ((t-body (if arg
					 `((let ((,arg j-exception)) ,@body))
					 body))
			     (th (cond ((listp target)
					(let (selector)
					  (dolist (tar target)
					    (push
					     (if (stringp tar)
						 `(%jeq j-exception-class
						       (java-class-real
							(find-java-class ,tar)))
						 `(%jeq j-exception-class
						       (java-class-real ,tar))
						 )
					     selector)
					    )
					  `((or ,@(nreverse selector))
					    ,@t-body)))
				       ((stringp target)
					`((%jeq j-exception-class
					       (java-class-real
						(find-java-class ,target)))
					  ,@t-body))
				       (t
					`((%jeq j-exception-class
					       (java-class-real ,target))
					  ,@t-body))
				       )))
			(push th code)))))
	   (t ;; if not handled we signal again.
	    ;(break "This is the resignaling case.")
	    (error condi)))))))


;;;
;;; Test case for handle-java-exception
;;; This use of handle-java-exception should produce what just follows it.
;;;

;; (handle-java-exception
;;  (FindClass "java/lang/Froboz")
;;  (java-NoClassDefFoundError
;;   (exception)
;;   (go 'next-try)
;;   )
;;  (("java.lang.ClassFormatError"
;;    "java.lang.ClassCircularityError"
;;    "java.lang.ExceptionInInitializerError")
;;   (exception)
;;   (error "JVM confused! It said: ~S.~%" exception)
;;   )
;;  ("java.lang.OutOfMemoryError"
;;   ()
;;   (format t "Game Over! JVM said we are out of memory!")
;;   (quit)
;;   )
;;  )
;;
;; (handler-case (FindClass "java/lang/Froboz")
;;   (java-exception (condi)
;;     ;(ExceptionClear)
;;     (let* ((j-exception (java-exception-thrown condi))
;; 	   )
;;       (cond ((%jeq j-exception (java-class-real java-NoClassDefFoundError))
;; 	     (let ((exception j-exception))
;; 	       (go 'next-try)
;; 	       )
;; 	     )
;; 	    ((or (%jeq j-exception
;;                    (java-class-real
;; 		       (find-java-class "java.lang.ClassFormatError"))
;; 		      )
;; 		 (%jeq j-exception
;;                    (java-class-real
;; 		       (find-java-class "java.lang.ClassCircularityError"))
;; 		      )
;; 		 (%jeq j-exception
;;                    (java-class-real
;; 		       (find-java-class "java.lang.ExceptionInInitializerError"))
;; 		      )
;; 		 )
;; 	     (let ((exception j-exception))
;; 	       (error "JVM confused! It said: ~S.~%" exception)
;; 	       )
;; 	     )
;; 	    ((%jeq j-exception
;;                (java-class-real
;; 		   (find-java-class "java.lang.OutOfMemoryError"))
;; 		  )
;; 	     (format t "Game Over! JVM said we are out of memory!")
;; 	     (quit)
;; 	     )
;; 	    )
;;       )
;;     )
;;   )


(defmacro jsynchronized (object &body body)
  `(let ((sync-obj ,object)
	 (lock-acquired nil))
     (unwind-protect
	  (progn (MonitorEnter sync-obj)
		 (setq lock-acquired t)
		 (let (sync-obj lock-acquired) ;; mask variables above
		   ,@body))
       (when lock-acquired
	 (MonitorExit sync-obj))))
  )

;;;
;;; Java contexts
;;;
;;; A Java context tries to adapt the idea of Java packages
;;; in a Common Lisp environment. Thus it is patterned a bit
;;; along the line of Common Lisp packages in its outer
;;; interface but implements the Java semantics on Java name lookup.
;;;

(defvar *all-java-contexts* nil)

(defvar +java-contexts-lock+ (bt:make-lock "All Java contexts"))

(defstruct (java-context (:constructor create-java-context))
  name
  single-type-imports  ;; a list of "Single import" Java types declarations.
  on-demand-imports ;; a list of "On demand" import declarations
  java-type-names ;; a cache of resolved class names (a hashtable).
  mutation-lock
  )

(defun make-java-context (name)
  (bt:with-lock-held (+java-contexts-lock+)
    (let ((context (find name *all-java-contexts*
			 :key #'java-context-name
			 :test #'string=)))
      (unless context
	(setq context (create-java-context :name name
					   :on-demand-imports '("java.lang.*")
					   :java-type-names
					   (make-hash-table :test #'equal)
					   :mutation-lock
					   (bt:make-lock (str+ "Java context: " name))))
	(push context *all-java-contexts*)
	)
      context
      )
    )
  )

(defvar *java-context* (make-java-context "java-user"))

(defun split-full-type-name (full-name)
  (let ((split-point (position #\. full-name :from-end t))
	)
    (if (null split-point)
	(values nil full-name)
	(values (subseq full-name 0 split-point)
		(subseq full-name (1+ split-point))))
    )
  )

(defmacro lookup-simple-type-name (context name)
  `(gethash ,name (java-context-java-type-names ,context))
  )

(defun register-simple-type-name (context full-name)
  (multiple-value-bind (package-part simple-name)
      (split-full-type-name full-name)
    (declare (ignore package-part))
    (let ((known (lookup-simple-type-name context simple-name)))
      (cond (known
	     (unless (string= full-name known)
	       (error "Java type import failure!~%   Attempt to import: ~S~%    clashes with: ~S~%"
		      full-name known)))
	    (t (setf (lookup-simple-type-name context simple-name) full-name))))))


(defun find-java-context (context-name)
  (declare (type string context-name))
  (find context-name *all-java-contexts* :key #'java-context-name :test #'string=))

(defmacro in-java-context (context-name)
  (let ((context (find-java-context context-name)))
    (unless context
      (setq context (make-java-context context-name))
      )
    (setq *java-context* context)
    )
  `(let ((context (find-java-context ,context-name)))
     (unless context
       (setq context (make-java-context ,context-name)))
     (setq *java-context* context)
     )
  )

(defmacro with-java-context (context-name &body body)
  `(let ((*java-context* (find-java-context ,context-name)))
     (declare (special *java-context*))
     ,@body
     )
  )



(defun delete-java-context (context)
  (declare (ignore context))
  (break "Not implemented yet: delete-java-context")
  )

(defun suffix-p (str suffix)
  (let* ((len-str (length str))
	 (len-suf (length suffix))
	 )
    (dotimes (i len-suf t)
      (unless (char= (char str (- len-str i 1)) (char suffix (- len-suf i 1)))
	(return-from suffix-p nil)
	)
      )
    )
  )

(defun is-on-demand-type-import-p (import-spec)
  (suffix-p import-spec ".*")
  )


(defmacro java-import (import-spec)
  (declare (string import-spec))
  (let ((*java-context* *java-context*))
    (unless *java-context*
      (error "No java context defined!~%"))
    (bt:with-lock-held ((java-context-mutation-lock *java-context*))
      (cond ((is-on-demand-type-import-p import-spec)
	     (pushnew import-spec
		      (java-context-on-demand-imports *java-context*)
		      :test #'string=)
	     )
	    (t ;; this must be  "Single Type Import Declaration".
	     (pushnew import-spec
		      (java-context-single-type-imports *java-context*)
		      :test #'string=)
	     ;; here we should also fill the hashtable of resolved names.
	     (register-simple-type-name *java-context* import-spec)
	     )
	    )
      )
    )
  `(let ((*java-context* *java-context*))
     (unless *java-context*
       (error "No java context defined!~%"))
     (bt:with-lock-held ((java-context-mutation-lock *java-context*))
       (cond ((is-on-demand-type-import-p ,import-spec)
	      (pushnew ,import-spec
		       (java-context-on-demand-imports *java-context*)
		       :test #'string=)
	      )
	     (t ;; this must be  "Single Type Import Declaration".
	      (pushnew ,import-spec
		       (java-context-single-type-imports *java-context*)
		       :test #'string=)
	      ;; here we should also fill the hashtable of resolved names.
	      (register-simple-type-name *java-context* ,import-spec)
	      )
	     )
       )
     )
  )

;;;
;;;  Java Reflection
;;;

;(defun java-array-length (array) (jfield-int array "length"))

(defun convert-java-array (jarray)
  (PushLocalFrame 2)
  (let* ((length (GetArrayLength jarray))
	 (vec (make-array (list length) :fill-pointer 0))
	 )
    (dotimes (i length)
      (let ((lref (GetObjectArrayElement jarray i)))
	(vector-push (NewGlobalRef lref) vec)
	(DeleteLocalRef lref)
	)
      )
    (PopLocalFrame (null-pointer))
    vec
    )
  )

(defun free-converted-java-array (an-array)
  (dotimes (i (length an-array))
    (when (aref an-array i)
      (DeleteGlobalRef (aref an-array i))
      (setf (aref an-array i) nil)
      )
    )
  )


;;;
;;;  Java Reflection caching
;;;

(defvar java-boolean-prim-type)
(defvar java-char-prim-type)
(defvar java-byte-prim-type)
(defvar java-short-prim-type)
(defvar java-int-prim-type)
(defvar java-long-prim-type)
(defvar java-float-prim-type)
(defvar java-double-prim-type)


(defun init-java-primitive-types ()
  (flet ((get-primitive-class (class-name)
	   (let ((lref (jfield-static-object class-name "TYPE" "Ljava/lang/Class;")))
	     (prog1 (NewGlobalRef lref) (DeleteLocalRef lref))))
	 )
    (setq java-boolean-prim-type (get-primitive-class "java/lang/Boolean"))
    (setq java-char-prim-type (get-primitive-class "java/lang/Character"))
    (setq java-byte-prim-type (get-primitive-class "java/lang/Byte"))
    (setq java-short-prim-type (get-primitive-class "java/lang/Short"))
    (setq java-int-prim-type (get-primitive-class "java/lang/Integer"))
    (setq java-long-prim-type (get-primitive-class "java/lang/Long"))
    (setq java-float-prim-type (get-primitive-class "java/lang/Float"))
    (setq java-double-prim-type (get-primitive-class "java/lang/Double"))
    )
  )

(defvar +java-reflection-lock+ (bt:make-lock "Java Reflection caching"))
;;;
;;; The global variable +java-types+ is the root of
;;; our Java Reflection caching.
;;;
;;; This is a true global and should not be dynamically bound.
;;;
;;; Its value is a hash-table that maps a class descriptor (aka: internal name)
;;; to its proper class proxy. It is a one-to-one mapping.
;;;
(defvar +java-types+ (make-hash-table :test #'equal))


(defstruct (java-field (:print-object print-java-field))
  name
  static
  real
  real-type
  id
  sig
  primitive-type
)

(defun print-java-field (jfield stream)
  (format stream "#S(JAVA-FIELD ~S ~S" (java-field-name jfield) (java-field-sig jfield))
  (when (java-field-static jfield)
    (format stream " static"))
  (format stream ")")
  )

(defstruct (java-method (:print-object print-java-method))
  name
  static
  real
  id
  sig
  return-ptype
  param-types
)

(defun print-java-method (jmethod stream)
  (format stream "#S(JAVA-METHOD ~S ~S"
	  (java-method-name jmethod)
	  (java-method-sig jmethod))
  (when (java-method-static jmethod)
    (format stream " static"))
  (format stream ")")
  )

;; (defstruct java-constructor
;;   name
;;   real
;;   id
;;   sig
;;   param-types
;;   )

(defstruct (java-class (:print-object print-java-class))
  name
  real
  fields
  methods
  static-fields
  static-methods
  constructors
  methods-fetched
  constructors-fetched
  )

(defun print-java-class (jclass stream)
  (format stream "#S(JAVA-CLASS ~S)" (java-class-name jclass))
  )

(defun java-string-to-lisp (jstr)
  (if (null-pointer-p jstr)
      nil
      (let ((len (GetStringLength jstr))
	    (utf-len (GetStringUTFLength jstr)))
	(debug-progn
	 (unless (= len utf-len)
	   (format t "~%Got a Java string with len = ~D and utf-len = ~D~%" len utf-len)
	   (finish-output)
	   (break)
	   )
	 )
	(with-foreign-object (buf :char (1+ utf-len))
	  (GetStringUTFRegion jstr 0 len buf)
	  (cffi:foreign-string-to-lisp buf :count utf-len) ;; this is post CFFI 0.10.X
	  )
	)
      ;; There is an alternative approach to this Java String convertion business.
      ;; We could have used Get/ReleaseStringCritical and get access to Unicode-16 chars
      ;; or even used Get/ReleaseStringUTFChars to do somewhat the same job in UTF.
      )
  )

(defun get-real-class-name (real)
  (PushLocalFrame 2)
  (let* ((jname (jmethod-object-0 real "getName" "()Ljava/lang/String;"))
	 )
    (prog1
	(java-string-to-lisp jname)
      (PopLocalFrame (null-pointer))) ;; Pops jname.
    )
  )


(defun Member-isStatic-p (member)
  (let* ((modif (jmethod-int-0 member "getModifiers"))
	 )
    (= JNI_TRUE (jmethod-static-boolean-1
		 (java-class-real java-Modifier) "isStatic" modif "(I)Z"))
    )
  )

(defun member-getName (member)
  (PushLocalFrame 2)
  (let* ((jname (jmethod-object-0 member "getName" "()Ljava/lang/String;"))
;;;	 (len (GetStringUTFLength jname))
;;;	 (buf (foreign-alloc :char :count (1+ len)))
	 )
;;;    (GetStringUTFRegion jname 0 len buf)
;;;    (PopLocalFrame (null-pointer))
;;;    (prog1 (foreign-string-to-lisp buf) (foreign-free buf))
    (prog1
	(java-string-to-lisp jname)
      (PopLocalFrame (null-pointer)))
    )
  )

(defun method-getParameterTypes (real)
  (PushLocalFrame 2)
  (prog1
      (convert-java-array
       (jmethod-object-0 real "getParameterTypes" "()[Ljava/lang/Class;"))
    (PopLocalFrame (null-pointer)))
  )

(defun method-getReturnType (real)
  (jmethod-object-0 real "getReturnType" "()Ljava/lang/Class;")
  )

(defun build-sig-for-method (method &key constructor)
  (PushLocalFrame 2)
  (let* ((real-method (java-method-real method))
	 (jarg-types (method-getParameterTypes real-method))
	 (jreturn-type (unless constructor
			 (method-getReturnType real-method)))
	 (jreturn-type-name (if constructor
				"void"
				(get-real-class-name jreturn-type)))
	 (args-sig "")
	 return-sig
	 )
    (cond ((string= "void" jreturn-type-name)
	   (setf return-sig "V" (java-method-return-ptype method) :void)
	   )
	  ((string= "int" jreturn-type-name)
	   (setf return-sig "I" (java-method-return-ptype method) 'jint)
	   )
	  ((string= "boolean" jreturn-type-name)
	   (setf return-sig "Z" (java-method-return-ptype method) 'jboolean)
	   )
	  ((string= "byte" jreturn-type-name)
	   (setf return-sig "B" (java-method-return-ptype method) 'jbyte)
	   )
	  ((string= "char" jreturn-type-name)
	   (setf return-sig "C" (java-method-return-ptype method) 'jchar)
	   )
	  ((string= "short" jreturn-type-name)
	   (setf return-sig "S" (java-method-return-ptype method) 'jshort)
	   )
	  ((string= "long" jreturn-type-name)
	   (setf return-sig "J" (java-method-return-ptype method) 'jlong)
	   )
	  ((string= "float" jreturn-type-name)
	   (setf return-sig "F" (java-method-return-ptype method) 'jfloat)
	   )
	  ((string= "double" jreturn-type-name)
	   (setf return-sig "D" (java-method-return-ptype method) 'jdouble)
	   )
	  (t ;; Here we got either an object reference or an array.
	   (replace-dots jreturn-type-name)
	   (unless (char= #\[ (schar jreturn-type-name 0)) ; this identifies an array!
	     ;; here we turn this object name into a cannonical class descriptor.
	     (setq jreturn-type-name (str+ "L" jreturn-type-name ";"))
	     )
	   (setf return-sig jreturn-type-name
		 (java-method-return-ptype method) 'jobject)
	   )
	  )
    (dotimes (i (length jarg-types))
      (let ((jarg-type-name (get-real-class-name (aref jarg-types i)))
	    )
	(cond ((string= "int" jarg-type-name)
	       (setq args-sig (str+ args-sig "I"))
	       )
	      ((string= "boolean" jarg-type-name)
	       (setq args-sig (str+ args-sig "Z"))
	       )
	      ((string= "byte" jarg-type-name)
	       (setq args-sig (str+ args-sig "B"))
	       )
	      ((string= "char" jarg-type-name)
	       (setq args-sig (str+ args-sig "C"))
	       )
	      ((string= "short" jarg-type-name)
	       (setq args-sig (str+ args-sig "S"))
	       )
	      ((string= "long" jarg-type-name)
	       (setq args-sig (str+ args-sig "J"))
	       )
	      ((string= "float" jarg-type-name)
	       (setq args-sig (str+ args-sig "F"))
	       )
	      ((string= "double" jarg-type-name)
	       (setq args-sig (str+ args-sig "D"))
	       )
	      (t ;; Here we got either an object reference or an array.
	       (replace-dots jarg-type-name)
	       (unless (char= #\[ (schar jarg-type-name 0)) ; this identifies an array!
		 ;; here we turn this object name into a cannonical class descriptor.
		 (setq jarg-type-name (str+ "L" jarg-type-name ";"))
		 )
	       (setq args-sig (str+ args-sig jarg-type-name))
	       )
	      ))
      )
    (setf (java-method-sig method) (str+ "(" args-sig ")" return-sig))
    (setf (java-method-param-types method) jarg-types)
    (PopLocalFrame (null-pointer))
    )
  )

(defun fetch-methods-of-class (class)
  (bt:with-lock-held (+java-reflection-lock+)
    (unless (java-class-methods-fetched class)
      (PushLocalFrame 2)
      (let* ((jmethods (jmethod-object-0 (java-class-real class)
					 "getMethods"
					 "()[Ljava/lang/reflect/Method;"))
	     (methods (convert-java-array jmethods))
	     (max (length methods))
	     instance-m
	     static-m
	     )
	(dotimes (i max)
	  (let ((jmethod (aref methods i))
		method
		)
	    (setq method (make-java-method :real jmethod
					   :name (member-getName jmethod)))
	    (setf (java-method-static method) (member-isStatic-p jmethod))
	    (build-sig-for-method method)
	    (setf (java-method-id method) (FromReflectedMethod jmethod))
	    (if (java-method-static method)
		(push method static-m)
		(push method instance-m)
		)
	    )
	  )
	(setf (java-class-methods class) instance-m)
	(setf (java-class-static-methods class) static-m)
	(setf (java-class-methods-fetched class) t)
	)
      (PopLocalFrame (null-pointer))
      )
    )
  )

(defun fetch-constructors-of-class (class)
  (bt:with-lock-held (+java-reflection-lock+)
    (unless (java-class-constructors-fetched class)
      (PushLocalFrame 2)
      (let* ((jconstructors (jmethod-object-0 (java-class-real class)
					      "getConstructors"
					      "()[Ljava/lang/reflect/Constructor;"))
	     (constructors (convert-java-array jconstructors))
	     set
	     )
	(dotimes (i (length constructors))
	  (let ((jconstructor (aref constructors i))
		constructor
		)
	    (setq constructor (make-java-method :real jconstructor
						:name (member-getName jconstructor)))
	    (setf (java-method-static constructor) nil)
	    (build-sig-for-method constructor :constructor t)
	    (setf (java-method-id constructor) (FromReflectedMethod jconstructor))
	    (push constructor set)
	    )
	  )
	(setf (java-class-constructors class) set)
	(setf (java-class-constructors-fetched class) t)
	)
      (PopLocalFrame (null-pointer))
      )
    )
  )

(defun fetch-java-class-from-jvm (internal-name)
  (let* ((real-class (FindClass internal-name))
	 (real-class-internal-name (replace-dots (get-real-class-name real-class)))
	 (prox-class (make-java-class :name real-class-internal-name
				      :real (NewGlobalRef real-class)))
	 )
    ;;(push prox-class +java-types+)
    (unless (string= internal-name real-class-internal-name)
      (break "Tried to find class ~S but found instead: ~S.~%"
	     internal-name real-class-internal-name))
    (setf (gethash real-class-internal-name +java-types+) prox-class)
    (DeleteLocalRef real-class)
    prox-class
    )
  )

(defun find-java-class-proxy-from-real (real)
  (let* ((class-internal-name (replace-dots (get-real-class-name real)))
	 (this ;;(find real +java-types+ :key #'java-class-real :test #'%jeq)
	  (gethash class-internal-name +java-types+)
	   )
	 )
    (unless this
      (bt:with-lock-held (+java-reflection-lock+)
	(unless (setq this ;;(find real +java-types+ :key #'java-class-real :test #'%jeq)
		      (gethash class-internal-name +java-types+)
		      )
	  (let* ((prox-class (make-java-class :name class-internal-name
					      ;;(get-real-class-name real)
					      :real (NewGlobalRef real)))
		 )
	    ;;(push prox-class +java-types+)
	    (setf (gethash class-internal-name +java-types+) prox-class)
	    (setq this prox-class)
	    )
	  )
	)
      )
    this
    )
  )

(defun find-java-class-proxy (internal-name)
  ;;(find internal-name +java-types+ :key #'java-class-name :test #'string=)
  (gethash internal-name +java-types+)
  )

(defun java-internal-full-type-name-p (name)
  (and (not (empty-str-p name))
       (or (not (null (position #\/ name))) (char= #\[ (char name 0))))
  )

(defun fetch-java-class-in-context (resolved-name name)
  (cond (resolved-name
	 (values (fetch-java-class-from-jvm resolved-name) resolved-name)
	 )
	((java-internal-full-type-name-p name)
	 (values (fetch-java-class-from-jvm name) name)
	 )
	(t
	 (let (try-name this this-name clashes)
	   (dolist (pkg-prefix (java-context-on-demand-imports *java-context*))
	     (setq pkg-prefix
		   (delete #\* (replace-dots (copy-seq pkg-prefix)) :from-end t))
	     (setq try-name (str+ pkg-prefix name))
	     ;;(break)
	     (handle-java-exception
	      (let ((class-found (find-java-class-proxy try-name))
		    ;; lookup proxy cache first because
		    ;; we may have already accessed the class
		    ;; by its full name without simple name resolution.
		    )
		(unless class-found
		  ;; Ok, we really don't know this one so we go to the JVM.
		  (setq class-found (fetch-java-class-from-jvm try-name)))
		;;(break)
		(if this
		    (push class-found clashes)
		    (setq this class-found this-name try-name)
		    )
		)
	      (java-NoClassDefFoundError
	       ()
	       ;;(break "First java handler.")
	       ;; This is a normal case. Let's loop to the next import spec.
	       )
;;; I don't think we should really handle this here.
;;; In fact, we would only be in the way or the real handler.
;;;
	      ;; 		(("java.lang.ClassFormatError"
	      ;; 		  "java.lang.ClassCircularityError"
	      ;; 		  "java.lang.ExceptionInInitializerError")
	      ;; 		 (exception)
	      ;; 		 (error "JVM internal error! It said: ~S.~%" exception)
	      ;; 		 )
	      ;; 		("java.lang.OutOfMemoryError"
	      ;; 		 ()
	      ;; 		 (error "Game Over! JVM said we are out of memory!")
	      ;; 		 )

	      )
	     )
	   (when clashes
	     (error "Ambiguous reference to ~S.~%Possible candidates are:~%~S~%"
		    name (mapcar #'get-real-class-name (cons this clashes)))
	     )
	   (unless this
	     (error "No Java class found for ~S.~%" name)
	     )
	   (values this this-name)
	   )
	 )
	)
;;;    )
  )

(defun java-full-type-name-p (name)
  (and (not (empty-str-p name))
       (or (not (null (position #\. name))) (char= #\[ (char name 0))))
  )

(defun find-java-class (name)
  (declare (string name))
  (unless (stringp name)
    (error "Value '~S' is not a valid Java class name." name))
  (let* ((*java-context* *java-context*) ;; to assure consistency through locks duration.
	 (internal-name (if (java-full-type-name-p name)
			    (replace-dots (copy-seq name))
			    (lookup-simple-type-name *java-context* name)))
	 (prox-class (find-java-class-proxy internal-name))
	 )
    (unless prox-class
      (let (full-internal-name)
	(bt:with-lock-held (+java-reflection-lock+)
	  (block java-reflection-update
	    (when (setq prox-class (find-java-class-proxy internal-name))
	      (return-from java-reflection-update)) ;; got beaten to it.

	    (multiple-value-setq (prox-class full-internal-name)
	      (fetch-java-class-in-context internal-name name))
	    )
	  )

	(unless internal-name
	  ;; this is the case of a simple name that is resolved for the first time.
	  ;; explicit array names go through this case too.
	  (unless (java-internal-full-type-name-p name)
	    (bt:with-lock-held ((java-context-mutation-lock *java-context*))
	      (let ((internal-name (lookup-simple-type-name *java-context* name)))
		(cond ((null internal-name)
		       (setf (lookup-simple-type-name *java-context* name)
			     full-internal-name)
		       )
		      ((string= internal-name full-internal-name)
		       ;; nothing to do, somebody else did it already.
		       )
		      (t
		       (error "Inconsistent Java class name mapping.~@
                               Name ~S is mapped to ~S and would become ~S.~%"
			      name internal-name full-internal-name)
		       )
		      )
		))
	    )
	  )
	)
      )
    prox-class
    )
  )

(defun arg-val-is-assignable-to (arg param)
  (when (jref-p arg) (setq arg (jref-it arg))) ;; unbox it first.
  (if (pointerp arg)
      (if (null-pointer-p arg)
	  ;; arg is a reference to a Java object.
	  (= JNI_FALSE (jmethod-boolean-0 param "isPrimitive"))
	  (prog2
	      (PushLocalFrame 2)
	      (= JNI_TRUE (IsAssignableFrom (GetObjectClass arg) param))
	    (PopLocalFrame (null-pointer)))
	  )
      (if (null arg)
	  (= JNI_FALSE (jmethod-boolean-0 param "isPrimitive"))
	  (typecase arg
	    (float
	     (or (%jeq param java-float-prim-type) (%jeq param java-double-prim-type))
	     )
	    (rational
	     ;; this is not very precise. Could be better! Depends on CFFI behaviour...
	     (or (%jeq param java-int-prim-type)
		 (%jeq param java-long-prim-type)
		 (%jeq param java-short-prim-type)
		 (%jeq param java-byte-prim-type)
		 (%jeq param java-float-prim-type)
		 (%jeq param java-double-prim-type)
		 )
	     )
	    (character
	     (or (%jeq param java-char-prim-type)
		 (%jeq param java-int-prim-type)
		 (%jeq param java-long-prim-type)
		 (%jeq param java-float-prim-type)
		 (%jeq param java-double-prim-type)
		 )
	     )
	    (jprim
	     (case (jprim-type arg) ;; This is the usual "widening" conversions.
	       (jboolean
		(%jeq param java-boolean-prim-type)
		)
	       (jchar
		(or (%jeq param java-char-prim-type)
		    (%jeq param java-int-prim-type)
		    (%jeq param java-long-prim-type)
		    (%jeq param java-float-prim-type)
		    (%jeq param java-double-prim-type)
		    )
		)
	       (jbyte
		(or (%jeq param java-int-prim-type)
		    (%jeq param java-long-prim-type)
		    (%jeq param java-short-prim-type)
		    (%jeq param java-byte-prim-type)
		    (%jeq param java-float-prim-type)
		    (%jeq param java-double-prim-type)
		    )
		)
	       (jshort
		(or (%jeq param java-int-prim-type)
		    (%jeq param java-long-prim-type)
		    (%jeq param java-short-prim-type)
		    (%jeq param java-float-prim-type)
		    (%jeq param java-double-prim-type)
		    )
		)
	       (jint
		(or (%jeq param java-int-prim-type)
		    (%jeq param java-long-prim-type)
		    (%jeq param java-float-prim-type)
		    (%jeq param java-double-prim-type)
		    )
		)
	       (jlong
		(or (%jeq param java-long-prim-type)
		    (%jeq param java-float-prim-type)
		    (%jeq param java-double-prim-type)
		    )
		)
	       (jfloat
		(or (%jeq param java-float-prim-type)
		    (%jeq param java-double-prim-type)
		    )
		)
	       (jdouble
		(%jeq param java-double-prim-type)
		)
	       )
	     )
	    (t
	     ;;(break "type is not assignable to a Java variable")
	     nil ;; this is not a type that can be assigned to a Java variable.
	     )
	    )
	  )
      )
  )

(defun is-applicable-method-p (method args)
  (let* ((params (java-method-param-types method))
	 (nb-params (array-total-size params))
	 (i 0)
	)
    (do ()
	((or (>= i nb-params) (null args)))
      (let ((param (aref params i))
	    (arg (car args))
	    )
	(unless (arg-val-is-assignable-to arg param)
	  ;;(break "reject method")
	  (return-from is-applicable-method-p nil)) ;; arg and param do not match.
	)
      (incf i)
      (pop args)
      )
    ;;(break "out of is-applicable-method-p")
    (if (and (= i nb-params) (endp args))
      (return-from is-applicable-method-p t) ;; everything matched.
      (return-from is-applicable-method-p nil) ;; ran out of either params or args.
      )
    )
  )

(defun is-more-specific-type (t1 t2)
  (cond ((%jeq t1 java-boolean-prim-type)
	 (%jeq t2 java-boolean-prim-type)
	 )
	((%jeq t1 java-char-prim-type)
	 (or (%jeq t2 java-char-prim-type)
	     (%jeq t2 java-int-prim-type)
	     (%jeq t2 java-long-prim-type)
	     (%jeq t2 java-float-prim-type)
	     (%jeq t2 java-double-prim-type)
	     )
	 )
	((%jeq t1 java-byte-prim-type)
	 (or (%jeq t2 java-int-prim-type)
	     (%jeq t2 java-long-prim-type)
	     (%jeq t2 java-short-prim-type)
	     (%jeq t2 java-byte-prim-type)
	     (%jeq t2 java-float-prim-type)
	     (%jeq t2 java-double-prim-type)
	     )
	 )
	((%jeq t1 java-short-prim-type)
	 (or (%jeq t2 java-int-prim-type)
	     (%jeq t2 java-long-prim-type)
	     (%jeq t2 java-short-prim-type)
	     (%jeq t2 java-float-prim-type)
	     (%jeq t2 java-double-prim-type)
	     )
	 )
	((%jeq t1 java-int-prim-type)
	 (or (%jeq t2 java-int-prim-type)
	     (%jeq t2 java-long-prim-type)
	     (%jeq t2 java-float-prim-type)
	     (%jeq t2 java-double-prim-type)
	     )
	 )
	((%jeq t1 java-long-prim-type)
	 (or (%jeq t2 java-long-prim-type)
	     (%jeq t2 java-float-prim-type)
	     (%jeq t2 java-double-prim-type)
	     )
	 )
	((%jeq t1 java-float-prim-type)
	 (or (%jeq t2 java-float-prim-type)
	     (%jeq t2 java-double-prim-type)
	     )
	 )
	((%jeq t1 java-double-prim-type)
	 (%jeq t2 java-double-prim-type)
	 )
	(t ;; this has to be an object reference
	 (= JNI_TRUE (IsAssignableFrom t1 t2))
	 )
	)
  )

(defun is-more-specific-method (m1 m2)
  (let ((params-1 (java-method-param-types m1))
	(params-2 (java-method-param-types m2))
	)
    (dotimes (i (array-total-size params-1))
      (let ((par1 (aref params-1 i))
	    (par2 (aref params-2 i)))
	(unless (is-more-specific-type par1 par2)
	  ;(break)
	  (return-from is-more-specific-method nil))
	)
      )
    ;(break)
    (return-from is-more-specific-method t)
    )
  )

(defun remove-from-list (this set)
  (if (eq this (car set))
      (cdr set)
      (do ((root set (cdr root))
	   )
	  ((endp (cdr root)) set)
	(when (eq this (cadr root))
	  (rplacd root (cddr root))
	  )
	)
      )
  )

(defun choose-most-specific-method (set)
  (do ((shrank t )
       )
      ((not shrank))
    (setq shrank nil)
    (do* ((head1 set (cdr head1))
	  (head2 set set)
	  (m1 (car head1) (car head1))
	  )
	 ((endp head1))
      (do* ((m2 (pop head2) (pop head2))
	    )
	   ((null m2))
	(when (and (not (eq m1 m2))
		   (is-more-specific-method m1 m2))
	  ;(break)
	  (setq set (remove-from-list m2 set))
	  (setq shrank t)
	  )
	)
      )
    )

  (when (cdr set)
    ;; this set is not a singleton at this point!
    ;;(break "Ambiguous invocation for method ~S." (java-method-name (car set)))
    (warn "Arbitrary resolution for method ~S.~%" (java-method-name (car set)))
    )
  ;(break)
  (car set)
  )

(defun choose-method-in-overload-set (set args)
  (let ((nb-args (length args))
	candidates
	)
    ;; first, select on the number of parameters.
    (dolist (method set)
      (when (= nb-args (length (java-method-param-types method)))
	(push method candidates)
	)
      )
    ;(break)
    (setq set candidates candidates nil)
    ;; second, match parameter types.
    (dolist (method set)
      (when (is-applicable-method-p method args)
	;;(break)
	(push method candidates))
	)
    ;(break)
    ;; there should be only one!
    (case (length candidates)
      (1 (car  candidates))
      (0 (error "No appropriate method found.~%"))
      (t (choose-most-specific-method candidates))
      )
    )
  )

(defun get-object-class (this)
  (unless (null-pointer-p this)
    (PushLocalFrame 2)
    (prog1
	(find-java-class-proxy-from-real (GetObjectClass this))
      (PopLocalFrame (null-pointer)))
    )
  )

(defun find-java-method (this name args)
  (let* ((class (get-object-class this))
	 (methods (java-class-methods class)))
    (unless methods
      (unless (java-class-methods-fetched class)
	(fetch-methods-of-class class))
      (setq methods (java-class-methods class))
      )
    (let (set)
      (dolist (method methods (setq set (nreverse set)))
	(when (string= name (java-method-name method))
	  ;(format t "Selecting method ~S as candidate." name)
	  (push method set))
	)
      (unless set
	(error "No instance method of name ~S on object of type ~S.~%"
	       name class)
	)
      (if (cdr set)
	  (choose-method-in-overload-set set args) ; Overloaded!
	  (car set)) ; Unambiguous.
      )
    )
  )

(defun find-java-static-method (this-class name args)
  (let* ((methods (java-class-static-methods this-class))
	 )
    (unless methods
      (unless (java-class-methods-fetched this-class)
	(fetch-methods-of-class this-class))
      (setq methods (java-class-static-methods this-class))
      )
    (let (set)
      (dolist (method methods (setq set (nreverse set)))
	(when (string= name (java-method-name method))
	  (push method set))
	)
      (unless set
	(error "No method of name ~S on class ~S.~%" name this-class)
	)
      (if (cdr set)
	  (choose-method-in-overload-set set args) ; Overloaded!
	  (car set)) ; Unambiguous.
      )
    )
  )

(defun find-java-constructor (class args)
  (let* ((constructors (java-class-constructors class))
	 ;;(name "<init>")
	 )
    (unless constructors
      (unless (java-class-constructors-fetched class)
	(fetch-constructors-of-class class))
      (setq constructors (java-class-constructors class)))
    (unless constructors
      (error "No constructor found for class ~S.~%" class)
      )
    (if (cdr constructors)
	(choose-method-in-overload-set constructors args)
	(car constructors))
    )
  )


#|
(defun fetch-java-field (real-class field-name)
;;   (PushLocalFrame 2)
;;   (PopLocalFrame
;;    (jmethod-object-1 real-class "getField" (NewStringUTF field-name)
;; 		     "(Ljava/lang/String;)Ljava/lang/reflect/Field;"))
  (with-jvm-local-frame (2 :local-ref-value t)
    (jmethod-object-1 real-class "getField" (NewStringUTF field-name)
		     "(Ljava/lang/String;)Ljava/lang/reflect/Field;"))
  )
|#

(defun Class-getField (real-class field-name)
  (jmethod-object-1 real-class "getField" field-name
		    "(Ljava/lang/String;)Ljava/lang/reflect/Field;")
  )

(defun replace-dots (descrip)
  (let ((len (length descrip)))
    (dotimes (i len)
      (when (char= #\. (schar descrip i))
	(setf (schar descrip i) #\/)
	)
      )
    )
  descrip
  )

(defun build-sig-for-field (field)
  (PushLocalFrame 2)
  (let* ((jfield (java-field-real field))
	 (jtype (jmethod-object-0 jfield "getType" "()Ljava/lang/Class;"))
	 (jtype-name (get-real-class-name jtype))
	 )
    (setf (java-field-real-type field) (NewGlobalRef jtype))
    (PopLocalFrame (null-pointer)) ;; Pops jtype
    (cond ((string= "int" jtype-name)
	   (setf (java-field-sig field) "I"
		 (java-field-primitive-type field) 'jint)
	   )
	  ((string= "boolean" jtype-name)
	   (setf (java-field-sig field) "Z"
		 (java-field-primitive-type field) 'jboolean)
	   )
	  ((string= "byte" jtype-name)
	   (setf (java-field-sig field) "B"
		 (java-field-primitive-type field) 'jbyte)
	   )
	  ((string= "char" jtype-name)
	   (setf (java-field-sig field) "C"
		 (java-field-primitive-type field) 'jchar)
	   )
	  ((string= "short" jtype-name)
	   (setf (java-field-sig field) "S"
		 (java-field-primitive-type field) 'jshort)
	   )
	  ((string= "long" jtype-name)
	   (setf (java-field-sig field) "J"
		 (java-field-primitive-type field) 'jlong)
	   )
	  ((string= "float" jtype-name)
	   (setf (java-field-sig field) "F"
		 (java-field-primitive-type field) 'jfloat)
	   )
	  ((string= "double" jtype-name)
	   (setf (java-field-sig field) "D"
		 (java-field-primitive-type field) 'jdouble)
	   )
	  (t ;; Here we got either an object reference or an array.
	   (replace-dots jtype-name)
	   (unless (char= #\[ (schar jtype-name 0)) ; this identifies an array!
	     ;; here we turn this object name into a cannonical class descriptor.
	     (setq jtype-name (str+ "L" jtype-name ";"))
	     )
	   (setf (java-field-sig field) jtype-name
		 (java-field-primitive-type field) 'jobject)
	   )
	  )
    )
  )

(defun find-java-static-field (class name)
  (let ((fields (java-class-static-fields class)))
    (let ((field (find name fields :key #'java-field-name :test #'string=)))
      (unless field
	(bt:with-lock-held (+java-reflection-lock+)
	  (block java-reflection-update
	    (when (setq field (find name fields :key #'java-field-name :test #'string=))
	      (return-from java-reflection-update)) ;; got beaten to it.

	    (with-jvm-local-frame (4)
	      (let ((jfield ;;(fetch-java-field (java-class-real class) name)))
		     (Class-getField (java-class-real class) (NewStringUTF name)));;2 lref
		    )
		(setq field (make-java-field :name name
					     :static (Member-isStatic-p jfield)
					     :real (NewGlobalRef jfield)
					     :id (FromReflectedField jfield)))
		(build-sig-for-field field)
		(if (java-field-static field)
		    (push field (java-class-static-fields class))
		    (push field (java-class-fields class)))
		)
	      )
	    )
	  )
	)
      field
      )
    )
  )

(defun find-java-field (class name)
  (let ((fields (java-class-fields class)))
    (let ((field (find name fields :key #'java-field-name :test #'string=)))
      (unless field
	(bt:with-lock-held (+java-reflection-lock+)
	  (block java-reflection-update
	    (when (setq field (find name fields :key #'java-field-name :test #'string=))
	      (return-from java-reflection-update)) ;; got beaten to it.

	    (with-jvm-local-frame (4)
	      (let ((jfield ;;(fetch-java-field (java-class-real class) name)))
		     (Class-getField (java-class-real class) (NewStringUTF name)));;2 lref
		    )
		(setq field (make-java-field :name name
					     :static (Member-isStatic-p jfield)
					     :real (NewGlobalRef jfield)
					     :id (FromReflectedField jfield)))
		(build-sig-for-field field)
		(if (java-field-static field)
		    (push field (java-class-static-fields class))
		    (push field (java-class-fields class)))
		)
	      )
	    )
	  )
	)
      field
      )
    )
  )

(defun jfield-static (class-designation name)
  (PushLocalFrame 2)
  (let* ((class (find-java-class class-designation))
	 (field (find-java-static-field class name))
	 (real-class (java-class-real class))
	 (field-id (java-field-id field))
	 result
	 )
    (unless (java-field-static field)
      (PopLocalFrame (null-pointer))
      (error "Field ~A is non-static but accessed as static." name))
    (case (java-field-primitive-type field)
      (jobject (setq result
		     (as-jref-value
		      (PopLocalFrame (GetStaticObjectField real-class field-id)))))
      (jint (setq result (GetStaticIntField real-class field-id))
	    (PopLocalFrame (null-pointer)))
      (jboolean (setq result (GetStaticBooleanField real-class field-id))
	    (PopLocalFrame (null-pointer)))
      (jbyte (setq result (GetStaticByteField real-class field-id))
	    (PopLocalFrame (null-pointer)))
      (jchar (setq result (GetStaticCharField real-class field-id))
	    (PopLocalFrame (null-pointer)))
      (jshort (setq result (GetStaticShortField real-class field-id))
	    (PopLocalFrame (null-pointer)))
      (jlong (setq result (GetStaticLongField real-class field-id))
	    (PopLocalFrame (null-pointer)))
      (jfloat (setq result (GetStaticFloatField real-class field-id))
	    (PopLocalFrame (null-pointer)))
      (jdouble (setq result (GetStaticDoubleField real-class field-id))
	    (PopLocalFrame (null-pointer)))
      (t (error "CL+J Internal error!"))
      )
    result
    )
  )

(defun jfield-instance (object name)
  (with-pinned-values (object)
    (when (jref-p object) (setq object (jref-it object)))
    (PushLocalFrame 2)
    (let* ((class (get-object-class object))
	   (field (find-java-field class name))
	   (field-id (java-field-id field))
	   result
	   )
      (when (java-field-static field)
	(PopLocalFrame (null-pointer))
	(error "Field ~A is static but accessed as non-static." name))
      (case (java-field-primitive-type field)
	(jobject (setq result
		       (as-jref-value
			(PopLocalFrame (GetObjectField object field-id)))))
	(jint (setq result (GetIntField object field-id))
	      (PopLocalFrame (null-pointer)))
	(jboolean (setq result (GetBooleanField object field-id))
		  (PopLocalFrame (null-pointer)))
	(jbyte (setq result (GetByteField object field-id))
	       (PopLocalFrame (null-pointer)))
	(jchar (setq result (GetCharField object field-id))
	       (PopLocalFrame (null-pointer)))
	(jshort (setq result (GetShortField object field-id))
		(PopLocalFrame (null-pointer)))
	(jlong (setq result (GetLongField object field-id))
	       (PopLocalFrame (null-pointer)))
	(jfloat (setq result (GetFloatField object field-id))
		(PopLocalFrame (null-pointer)))
	(jdouble (setq result (GetDoubleField object field-id))
		 (PopLocalFrame (null-pointer)))
	(t (error "CL+J Internal error!"))
	)
      result
      )
    )
  )

;; ;; Is there any real use of this jfield macro?
;; ;; It seems not but let's keep it for a while. (2009-04-25)
;; (defmacro jfield (java-object-ref name)
;;   (if (stringp java-object-ref)
;;       `(jfield-static ,java-object-ref ,name)
;;       `(jfield-instance ,java-object-ref ,name)
;;       )
;;   )

(defun jfield-static-set (class-designation name value)
  (let* ((class (find-java-class class-designation))
	 (field (find-java-static-field class name))
	 (real-class (java-class-real class))
	 (field-id (java-field-id field))
	 )
    (unless (java-field-static field)
      (error "Field ~A is non-static but accessed as static." name))
    (case (java-field-primitive-type field)
      (jobject
       (with-pinned-values (value)
	 (PushLocalFrame 4)
	 (when (jref-p value) (setq value (jref-it value)))
	 (if (and (pointerp value)
		  (or (null-pointer-p value)
		      (= JNI_TRUE (IsAssignableFrom (GetObjectClass value)
						    (java-field-real-type field)))))
	     (SetStaticObjectField real-class field-id value)
	     (progn
	       (PopLocalFrame (null-pointer))
	       (error "Static field ~A of class ~A cannot be assigned value: ~S."
		      name (java-class-name class) value)))
	 (PopLocalFrame (null-pointer))
	 )
       )
      (jint (SetStaticIntField real-class field-id value))
      (jboolean (SetStaticBooleanField real-class field-id value))
      (jbyte (SetStaticByteField real-class field-id value))
      (jchar (SetStaticCharField real-class field-id value))
      (jshort (SetStaticShortField real-class field-id value))
      (jlong (SetStaticLongField real-class field-id value))
      (jfloat (SetStaticFloatField real-class field-id value))
      (jdouble (SetStaticDoubleField real-class field-id value))
      (t (error "CL+J Internal error!"))
      )
    value
    )
  )

(defun jfield-instance-set (java-object name value)
  (with-pinned-values (java-object)
    (when (jref-p java-object) (setq java-object (jref-it java-object)))
    (let* ((class (get-object-class java-object))
	   (field (find-java-field class name))
	   (field-id (java-field-id field))
	   )
      (when (java-field-static field)
	(error "Field ~A is static but accessed as non-static." name))
      (case (java-field-primitive-type field)
	(jobject
	 (with-pinned-values (value)
	   (PushLocalFrame 4)
	   (when (jref-p value) (setq value (jref-it value)))
	   (if (and (pointerp value)
		    (or (null-pointer-p value)
			(= JNI_TRUE (IsAssignableFrom (GetObjectClass value)
						      (java-field-real-type field)))))
	       (SetObjectField java-object field-id value)
	       (progn
		 (PopLocalFrame (null-pointer))
		 (error "Instance field ~A of object of class ~A ~
                         cannot be assigned value: ~S."
			name (java-class-name class) value)
		 )
	       )
	   (PopLocalFrame (null-pointer))
	   )
	 )
	(jint (SetIntField java-object field-id value))
	(jboolean (SetBooleanField java-object field-id value))
	(jbyte (SetByteField java-object field-id value))
	(jchar (SetCharField java-object field-id value))
	(jshort (SetShortField java-object field-id value))
	(jlong (SetLongField java-object field-id value))
	(jfloat (SetFloatField java-object field-id value))
	(jdouble (SetDoubleField java-object field-id value))
	(t (error "CL+J Internal error!"))
	)
      value
      )
    )
  )



;;;;;


(defun jmethod-instance (this method-name &rest args)
  ;(declare (ignore this method-name args))
  ;;(declare (special this args)) ;; to pin them againt GC for the rest of the function.
  ;;(let ((this this) (args args)) ;; to be free to modify them without unpinning them.
  (with-pinned-values (this args)
    (when (jref-p this)
      (setq this (jref-it this)))
    (unless (cffi:pointerp this)
      (error "Cannot invoke Java method ~A on Lisp object ~S." method-name this))
    (when (cffi:null-pointer-p this)
      (error "While invoking Java method ~S; No method can be invoked on Java object reference null." method-name))
    (let* ((method (find-java-method this method-name args))
	   (id (java-method-id method))
	   (return-ptype (java-method-return-ptype method))
	   (sig (java-method-sig method))
	   )
      (unless return-ptype
	(build-sig-for-method method)
	(setq return-ptype (java-method-return-ptype method))
	(setq sig (java-method-sig method))
	)
      (with-foreign-object (jargs (union-jvalue) (+ 2 (length args)))
	(dyn-fill-args sig jargs args)
	(case return-ptype
	  (jobject (as-jref-value (CallObjectMethodA this id jargs)))
	  (jint (CallIntMethodA this id jargs))
	  (:void (CallVoidMethodA this id jargs))
	  (jboolean (CallBooleanMethodA this id jargs))
	  (jbyte (CallByteMethodA this id jargs))
	  (jchar (CallCharMethodA this id jargs))
	  (jshort (CallShortMethodA this id jargs))
	  (jlong (CallLongMethodA this id jargs))
	  (jfloat (CallFloatMethodA this id jargs))
	  (jdouble (CallDoubleMethodA this id jargs))
	  (t (error "CL+J: Internal error while calling ~S.~%" method-name))
	  )
	)
      )
    )
  )

(defun jmethod-static (this-class method-name &rest args)
  ;;(declare (special args)) ;; to pin them against GC for the rest of the function.
  ;;(let ((args args)) ;; to be free to modify it without unpinning it.
  (with-pinned-values (args)
    (let* ((method (find-java-static-method this-class method-name args))
	   (this-real-class (java-class-real this-class))
	   (id (java-method-id method))
	   (return-ptype (java-method-return-ptype method))
	   (sig (java-method-sig method))
	   )
      (unless return-ptype
	(build-sig-for-method method)
	(setq return-ptype (java-method-return-ptype method))
	(setq sig (java-method-sig method))
	)
      (with-foreign-object (jargs (union-jvalue) (length args))
	(dyn-fill-args sig jargs args)
	(case return-ptype
	  (jobject (as-jref-value (CallStaticObjectMethodA this-real-class id jargs)))
	  (jint (CallStaticIntMethodA this-real-class id jargs))
	  (:void (CallStaticVoidMethodA this-real-class id jargs))
	  (jboolean (CallStaticBooleanMethodA this-real-class id jargs))
	  (jbyte (CallStaticByteMethodA this-real-class id jargs))
	  (jchar (CallStaticCharMethodA this-real-class id jargs))
	  (jshort (CallStaticShortMethodA this-real-class id jargs))
	  (jlong (CallStaticLongMethodA this-real-class id jargs))
	  (jfloat (CallStaticFloatMethodA this-real-class id jargs))
	  (jdouble (CallStaticDoubleMethodA this-real-class id jargs))
	  (t (error "CL+J: Internal error while calling ~S.~%" method-name))
	  )
	)
      )
    )
  )

(defmacro jmethod (this method-name &rest args)
  (cond ((stringp this)
	 `(jmethod-static (find-java-class ,this) ,method-name ,@args)
	 )
	(t
	 `(jmethod-instance ,this ,method-name ,@args)
	 )
	)
  )

;;;
;;;
;;;

(defun jalloc-and-construct (class args)
  (let* ((constructor (find-java-constructor class args))
	 (real-class (java-class-real class))
	 (id (java-method-id constructor))
	 (sig (java-method-sig constructor))
	 )
    (with-foreign-object (jargs (union-jvalue) (length args))
      (dyn-fill-args sig jargs args)
      (as-jref-value (NewObjectA real-class id jargs))
      )
    )
  )

(defun jnew (java-class &rest args)
  (cond
    ((stringp java-class)
     (jalloc-and-construct (find-java-class java-class) args)
     )
    ((pointerp java-class)
     (jalloc-and-construct (find-java-class-proxy-from-real java-class) args)
     )
    ((jref-p java-class)
     (jalloc-and-construct (find-java-class-proxy-from-real (jref-it java-class)) args)
     )
    (t
     (error "Invalid first argument to jnew."))
    )
  )

(defun jstr (some-lisp-string)
  (declare (string some-lisp-string))
  (as-jref-value (NewStringUTF some-lisp-string))
  )

(defmacro jstring (x) `(jstr ,x)) ;; just for convenience.

(defun jstring-length (jstr)
  (with-pinned-values (jstr) ;;let ((pinned-jstr jstr)) (declare (special pinned-jstr))

    (when (jref-p jstr) (setq jstr (jref-it jstr))) ;; unbox it first.

    ;; We must make sure 'jstr' is a valid
    ;; Java String, otherwise the JVM blows up!
    (unless (pointerp jstr)
      (error "Argument to jstring-length is not a Java object.")
      )
    (unless (with-jvm-local-frame (2)
	      (%jeq (GetObjectClass jstr) (java-class-real java-String)))
      (error "Argument to jstring-length is not a java.lang.String")
      )
    (GetStringLength jstr)
    )
  )

(defun jnew-object[] (component-type dim)
  ;; component-type must be a Java Class reference.
  ;;(PushLocalFrame 6)
  (with-jvm-local-frame (6)
    (unless (and (pointerp component-type)
		 (%jeq (java-class-real java-Class) (GetObjectClass component-type)))
      (error "Java array component type must be a proper class.~% Got this instead: ~S~%"
	     (prog1 (java-string-to-lisp
		     (jmethod-object-0 component-type "toString" "()Ljava/lang/String;"))
	       #+nil (PopLocalFrame (null-pointer))))
      )
    (let ((vec (NewObjectArray dim component-type (null-pointer))))
      (prog1 (jref vec) #+nil (PopLocalFrame (null-pointer))))
    )
  )

(defun jnew-boolean[] (dim)
  ;;(PushLocalFrame 2)
  (with-jvm-local-frame (2)
    (let ((vec (NewBooleanArray dim)))
      (prog1 (jref vec) #+nil (PopLocalFrame (null-pointer))))
    )
  )

(defun jnew-byte[] (dim)
  ;;(PushLocalFrame 2)
  (with-jvm-local-frame (2)
    (let ((vec (NewByteArray dim)))
      (prog1 (jref vec) #+nil (PopLocalFrame (null-pointer))))
    )
  )

(defun jnew-char[] (dim)
  ;;(PushLocalFrame 2)
  (with-jvm-local-frame (2)
    (let ((vec (NewCharArray dim)))
      (prog1 (jref vec) #+nil (PopLocalFrame (null-pointer))))
    )
  )

(defun jnew-short[] (dim)
  ;;(PushLocalFrame 2)
  (with-jvm-local-frame (2)
    (let ((vec (NewShortArray dim)))
      (prog1 (jref vec) #+nil (PopLocalFrame (null-pointer))))
    )
  )

(defun jnew-int[] (dim)
  ;;(PushLocalFrame 2)
  (with-jvm-local-frame (2)
    (let ((vec (NewIntArray dim)))
      (prog1 (jref vec) #+nil (PopLocalFrame (null-pointer))))
    )
  )

(defun jnew-long[] (dim)
  ;;(PushLocalFrame 2)
  (with-jvm-local-frame (2)
    (let ((vec (NewLongArray dim)))
      (prog1 (jref vec) #+nil (PopLocalFrame (null-pointer))))
    )
  )

(defun jnew-float[] (dim)
  ;;(PushLocalFrame 2)
  (with-jvm-local-frame (2)
    (let ((vec (NewFloatArray dim)))
      (prog1 (jref vec) #+nil (PopLocalFrame (null-pointer))))
    )
  )

(defun jnew-double[] (dim)
  ;;(PushLocalFrame 2)
  (with-jvm-local-frame (2)
    (let ((vec (NewDoubleArray dim)))
      (prog1 (jref vec) #+nil (PopLocalFrame (null-pointer))))
    )
  )



(defun jnew[] (component-type dims)
  (let (dim) ;; vec)
    (cond ((listp dims)
	   (setq dim (pop dims))
	   (unless (integerp dim)
	     (error "Invalid Java array dimension: ~S~% Dimension must be an integer.~%"
		    dim))
	   )
	  ((integerp dims)
	   (setq dim dims dims nil)
	   )
	  (t
	   (error "Invalid second argument to jnew. ~S~% ~
                   Dimensions must be an integer or a list of integer.~%"
		  dims)
	   )
	  )

    (cond ((endp dims)
	   ;; this is a vector.
	   (cond ((stringp component-type)
		  ;; This must be a type name.
		  (cond ((string= component-type "boolean") (jnew-boolean[] dim))
			((string= component-type "byte") (jnew-byte[] dim))
			((string= component-type "char") (jnew-char[] dim))
			((string= component-type "short") (jnew-short[] dim))
			((string= component-type "int") (jnew-int[] dim))
			((string= component-type "long") (jnew-long[] dim))
			((string= component-type "float") (jnew-float[] dim))
			((string= component-type "double") (jnew-double[] dim))
			(t
			 (let ((comp-class (find-java-class component-type)))
			   (if comp-class
			       (jnew-object[] (java-class-real comp-class) dim)
			       (error "Unknown array component class: ~S~%"
				      component-type))
			   )
			 )
			)
		  )
		 (t
		  ;; This must be a type reference.
		  (cond
		    ((%jeq component-type java-boolean-prim-type) (jnew-boolean[] dim))
		    ((%jeq component-type java-char-prim-type) (jnew-char[] dim))
		    ((%jeq component-type java-byte-prim-type) (jnew-byte[] dim))
		    ((%jeq component-type java-short-prim-type) (jnew-short[] dim))
		    ((%jeq component-type java-int-prim-type) (jnew-int[] dim))
		    ((%jeq component-type java-long-prim-type) (jnew-long[] dim))
		    ((%jeq component-type java-float-prim-type) (jnew-float[] dim))
		    ((%jeq component-type java-double-prim-type) (jnew-double[] dim))
		    (t (jnew-object[] component-type dim))
		    )
		  )
		 )
	   )
	  (t
	   ;; this is a multi-dimensional array.
	   (let (vec)
	     ;;(PushLocalFrame 4)
	     (with-jvm-local-frame (4) ;; the depth is 2 but as usual we double.
	       (dotimes (i dim)
		 (let ((comp (jnew[] component-type dims)))
		   (unless vec
		     (let ((comp-class (GetObjectClass (jref-it comp)))) ;; lref
		       (setq vec (NewObjectArray dim comp-class (null-pointer))) ;; lref
		       (DeleteLocalRef comp-class)
		       )
		     )
		   (SetObjectArrayElement vec i (jref-it comp))
		   )
		 )
	       (setq vec (jref vec))
	       )
	     ;;(PopLocalFrame (null-pointer))
	     vec
	     )
	   )
	  )
    ;;vec
    )
  )

(defun jaref (java-array &rest subscripts)
  (with-pinned-values (java-array)
    ;;let ((pinned-java-array java-array)) (declare (special pinned-java-array))

    (when (jref-p java-array) (setq java-array (jref-it java-array))) ;; unbox it first.

    (when (null-pointer-p java-array)
      (return-from jaref nil))

    (PushLocalFrame 6)

    (let* ((subscript (pop subscripts))
	   (array-class (GetObjectClass java-array)) ;; lref
	   comp-type
	   val
	   sub-array
	   ;;(depth 0)
	   )
      (declare (integer subscript))
      (unless (= JNI_TRUE (jmethod-instance array-class "isArray"))
	(PopLocalFrame (null-pointer))
	(error "First argument to jaref is not a Java Array object.")
	)

      (setq comp-type (jmethod-object-0 array-class
					"getComponentType"
					"()Ljava/lang/Class;")) ;; lref

      (do ()
	  ((= JNI_TRUE (jmethod-boolean-0 comp-type "isPrimitive")))

	;;(format t "~%depth = ~D, index = ~D." depth subscript) (finish-output)
	;;(incf depth)

	;; Here we know that the component type must be Object.

	;;(format t "~%About to get array element.") (finish-output)
	(setq val (GetObjectArrayElement java-array subscript)) ;; this is a LocalRef.
	(when sub-array
	  (DeleteLocalRef sub-array)) ;; clean left-overs of the previous iteration.

	(cond ((and (= JNI_TRUE (jmethod-boolean-0 comp-type "isArray"))
		    (not (endp subscripts)))
	       ;; Prepare next iteration
	       (setq sub-array val
		     java-array sub-array
		     array-class comp-type
		     comp-type (prog1
				   (jmethod-object-0 comp-type
						     "getComponentType"
						     "()Ljava/lang/Class;")
				 (DeleteLocalRef comp-type))
		     subscript (pop subscripts))
	       )
	      (t
	       (unless (endp subscripts)
		 (PopLocalFrame (null-pointer))
		 (error "Too many subscripts on Java array.")
		 )
	       (setq val (jref val))
	       (PopLocalFrame (null-pointer))
	       (return-from jaref val)
	       )
	      )
	)

      ;;(format t "~%depth = ~D, index = ~D final." depth subscript) (finish-output)

      ;; If we get here we know that comp-type is a Java primitive type.

      (cond ((%jeq comp-type java-boolean-prim-type)
	     (with-foreign-object (buf 'jboolean 2)
	       (GetBooleanArrayRegion java-array subscript 1 buf)
	       (setq val (mem-ref buf 'jboolean))
	       )
	     )
	    ((%jeq comp-type java-char-prim-type)
	     (with-foreign-object (buf 'jchar 2)
	       (GetCharArrayRegion java-array subscript 1 buf)
	       (setq val (mem-ref buf 'jchar))
	       )
	     )
	    ((%jeq comp-type java-byte-prim-type)
	     (with-foreign-object (buf 'jbyte 2)
	       (GetByteArrayRegion java-array subscript 1 buf)
	       (setq val (mem-ref buf 'jbyte))
	       )
	     )
	    ((%jeq comp-type java-short-prim-type)
	     (with-foreign-object (buf 'jshort 2)
	       (GetShortArrayRegion java-array subscript 1 buf)
	       (setq val (mem-ref buf 'jshort))
	       )
	     )
	    ((%jeq comp-type java-int-prim-type)
	     (with-foreign-object (buf 'jint 2)
	       (GetIntArrayRegion java-array subscript 1 buf)
	       (setq val (mem-ref buf 'jint))
	       )
	     )
	    ((%jeq comp-type java-long-prim-type)
	     (with-foreign-object (buf 'jlong 2)
	       (GetLongArrayRegion java-array subscript 1 buf)
	       (setq val (mem-ref buf 'jlong))
	       )
	     )
	    ((%jeq comp-type java-float-prim-type)
	     (with-foreign-object (buf 'jfloat 2)
	       (GetFloatArrayRegion java-array subscript 1 buf)
	       (setq val (mem-ref buf 'jfloat))
	       )
	     )
	    ((%jeq comp-type java-double-prim-type)
	     (with-foreign-object (buf 'jdouble 2)
	       (GetDoubleArrayRegion java-array subscript 1 buf)
	       (setq val (mem-ref buf 'jdouble))
	       )
	     )
	    (t
	     (PopLocalFrame (null-pointer))
	     (error "Internal cl+j error! Unknown Java primitive type in array reference.")
	     )
	    )
      (PopLocalFrame (null-pointer))
      (return-from jaref val)
      )
    )
  )

(defun jaref-set (java-array value &rest subscripts)
  (with-pinned-values (java-array)
    (let ((pre-subscripts (butlast subscripts))
	  (subscript (car (last subscripts)))
	  ;;(pinned-java-array java-array)
	  ;;(pinned-value value)
	  (value value)
	  )
      ;;(declare (special pinned-java-array pinned-value))

      (when (jref-p java-array) (setq java-array (jref-it java-array))) ;; unbox it first.
      (when (jref-p value) (setq value (jref-it value)))

      (when pre-subscripts
	(let ((target-vec (apply #'jaref java-array pre-subscripts))) ;; jref
	  (if target-vec
	      (setq java-array (jref-it target-vec))
	      (error "Invalid Java array update location.")
	      )
	  )
	)

      (with-jvm-local-frame (6)
	(let* ((vec-class (GetObjectClass java-array)) ;; lref
	       (comp-type (jmethod-object-0 vec-class
					    "getComponentType"
					    "()Ljava/lang/Class;")) ;; lref
	       ;;value-type
	       )
#|
	  It turns out that SetObjectArrayElement does the assignment
	  compatibility check already. We would only do double duty here!

	  ;; We must make sure that the "value" type and the component type
	  ;; are assignment compatible.
	  (cond ((pointerp value)
		 (if (null-pointer-p value)
		     (setq value-type (null-pointer))
		     (setq value-type (GetObjectClass value)) ;; lref
		     )
		 )
		((jprim-p value)
		 (setq value-type
		       (case (jprim-type value)
			 (jboolean java-boolean-prim-type)
			 (jchar java-char-prim-type)
			 (jbyte java-byte-prim-type)
			 (jshort java-short-prim-type)
			 (jint java-int-prim-type)
			 (jlong java-long-prim-type)
			 (jfloat java-float-prim-type)
			 (jdouble java-double-prim-type)
			 (t
			  (PopLocalFrame (null-pointer))
			  (error "Internal CL+J error. Invalid Java primitive type.")
			  )
			 )
		       )
		 )
		((numberp value) ;; byte, short, int, long, float, double
		 )
		((characterp value) ;; char
		 )
		((or (null value) (eq t value)) ;; boolean
		 )
		(t
		 (PopLocalFrame (null-pointer))
		 (error
		  "This value cannot be a meaningfull element of a Java array: ~S~%."
		  value)
		 )
		)
	  #+ ccl (identity value-type) ;; just to silence CCL.
|#
	  (cond ((/= JNI_TRUE (jmethod-boolean-0 comp-type "isPrimitive"))
		 (SetObjectArrayElement java-array subscript value)
		 )
		((%jeq comp-type java-boolean-prim-type)
		 (with-foreign-object (buf 'jboolean 2)
		   (setf (mem-aref buf 'jboolean) value)
		   (SetBooleanArrayRegion java-array subscript 1 buf)
		   )
		 )
		((%jeq comp-type java-char-prim-type)
		 (with-foreign-object (buf 'jchar 2)
		   (setf (mem-aref buf 'jchar) value)
		   (SetCharArrayRegion java-array subscript 1 buf)
		   )
		 )
		((%jeq comp-type java-byte-prim-type)
		 (with-foreign-object (buf 'jbyte 2)
		   (setf (mem-aref buf 'jbyte) value)
		   (SetByteArrayRegion java-array subscript 1 buf)
		   )
		 )
		((%jeq comp-type java-short-prim-type)
		 (with-foreign-object (buf 'jshort 2)
		   (setf (mem-aref buf 'jshort) value)
		   (SetShortArrayRegion java-array subscript 1 buf)
		   )
		 )
		((%jeq comp-type java-int-prim-type)
		 (with-foreign-object (buf 'jint 2)
		   (setf (mem-aref buf 'jint) value)
		   (SetIntArrayRegion java-array subscript 1 buf)
		   )
		 )
		((%jeq comp-type java-long-prim-type)
		 (with-foreign-object (buf 'jlong 2)
		   (setf (mem-aref buf 'jlong) value)
		   (SetLongArrayRegion java-array subscript 1 buf)
		   )
		 )
		((%jeq comp-type java-float-prim-type)
		 (with-foreign-object (buf 'jfloat 2)
		   (setf (mem-aref buf 'jfloat) value)
		   (SetFloatArrayRegion java-array subscript 1 buf)
		   )
		 )
		((%jeq comp-type java-double-prim-type)
		 (with-foreign-object (buf 'jdouble 2)
		   (setf (mem-aref buf 'jdouble) value)
		   (SetDoubleArrayRegion java-array subscript 1 buf)
		   )
		 )
		(t
		 (error "Internal CL+J error. Unknown Java primitive type.")
		 )
		)
	  )
	)
      )
    )
  value
  )

(defsetf jaref (java-array &rest subscripts) (val)
  `(jaref-set ,java-array ,val ,@subscripts)
  )

;;; A test case for the defsetf above: (setf (jaref foobar 2 3 4 ) 1)

(defun jlength (java-array)
  (with-pinned-values (java-array)
    ;;let ((pinned-java-array java-array)) (declare (special pinned-java-array))

    (when (jref-p java-array) (setq java-array (jref-it java-array))) ;; unbox it first.

    ;; We must make sure 'java-array' is a valid
    ;; Java Array object, otherwise the JVM blows up!
    (unless (pointerp java-array)
      (error "Argument to jlength is not a Java object."))
    (unless (with-jvm-local-frame (2)
		(= JNI_TRUE (jmethod-instance (GetObjectClass java-array) "isArray")))
      (error "Argument to jlength is not a Java Array object."))
    (GetArrayLength java-array)
    )
  )

;;;
;;;
;;;

(defvar +java-to-lisp-converters+ (make-hash-table))
(defvar +converters-lock+ (bt:make-lock "CL+J Converters"))
;;(defvar *lisp-to-java-converters* (make-hash-table))

(defun add-jtol-mapping (jclass converter)
  (when (weak-jref-p jclass) (setq jclass (jref jclass)))
  (when (jref-p jclass) (setq jclass (jref-it jclass)))
  (when (cffi:pointerp jclass)
    (setq jclass (find-java-class-proxy-from-real jclass)))
  (when (java-class-p jclass)
    (bt:with-lock-held (+converters-lock+)
      (setf (gethash jclass +java-to-lisp-converters+) converter)))
  )

(defun delete-jtol-mapping (jclass)
  (when (weak-jref-p jclass) (setq jclass (jref jclass)))
  (when (jref-p jclass) (setq jclass (jref-it jclass)))
  (when (cffi:pointerp jclass)
    (setq jclass (find-java-class-proxy-from-real jclass)))
  (when (java-class-p jclass)
    (bt:with-lock-held (+converters-lock+)
      (remhash jclass +java-to-lisp-converters+)))
  )

(defun jtol (jobj)
  (when (weak-jref-p jobj) (setq jobj (jref jobj)))
  (when (jref-p jobj) (setq jobj (jref-it jobj)))
  (if (cffi::pointerp jobj)
      (if (cffi::null-pointer-p jobj)
	  nil
	  (let* ((jclass (get-object-class jobj))
		 (converter (gethash jclass +java-to-lisp-converters+))
		 )
	    (if converter
		(funcall converter jobj)
		(error "Don't know how to convert Java objects of class ~S.~%~
                        Try to add a new converter to jtol using add-jtol-mapping." jclass))
	    )
	  )
      (if (jprim-p jobj)
	  (mem-ref (jprim-value jobj) (jprim-type jobj))
	  jobj))
  )


(defgeneric ltoj (lobj))

(defmethod ltoj ((lobj t))
  (if (null lobj)
      jnull
      (let ((suggest (typecase lobj
		       ;; (mkcl:integer8 "using jprim-byte")
		       ;; (mkcl:integer16 "using jprim-short")
		       ;; (mkcl:integer32 "using jprim-int")
		       ;; (mkcl:integer64 "using jprim-long")
		       ((signed-byte 8) "using jprim-byte")
		       ((signed-byte 16) "using jprim-short")
		       ((signed-byte 32) "using jprim-int")
		       ((signed-byte 64) "using jprim-long")
		       (single-float "using jprim-float")
		       (double-float "using jprim-double")
		       (character "using jprim-char")
		       (t (if (eq lobj t)
			      "using jprim-boolean"
			      "coding a new method for generic function ltoj")))))
	(error "Don't know how to convert Lisp object ~S to a Java object. Consider ~A." lobj suggest)
	)
      )
  )

(defmethod ltoj ((str string))
  (jstring str))


(defun jtol-init ()
  (add-jtol-mapping (find-java-class "java.lang.String") #'java-string-to-lisp)

  ;; later on, once this file gets loaded you could do this instead:
  ;; (add-jtol-mapping #?java.lang.String.class #'java-string-to-lisp)
  )

;;;
;;;
;;;

(define-condition java-throwable (condition)
  ((thrown :initarg :thrown
	   :reader java-throwable-thrown))
  ;;(:report print-java-throwable)
  (:report (lambda (condition stream)
	     (format stream "Java throwable: ~S.~%"
		     (jtol (jmethod-instance
			    (java-throwable-thrown condition) "toString")))
	     )
	   )
  )

(export '(java-throwable java-throwable-thrown))


;; (defun print-java-throwable (condition stream)
;;   ;;(ExceptionClear) ;; Do we really need to do this here?
;;   (format stream "Java exception: ~S.~%"
;; 	  (throwable-toString
;; 	   (java-throwable-thrown condition)))
;;   )


(defmacro throw-java-exception (jclass &rest args)
  `(let* ((j-excep (jnew ,jclass ,@args))
	  (cond-wrapper (make-condition 'java-throwable :thrown j-excep))
	  )

     (signal cond-wrapper)
     (error "Java exception ~S was not handled!~%" j-excep)
     )
  )


;;;
;;;
;;;

(defun java-alpha-char-p (ch)
  (or (alpha-char-p ch) (char= ch #\_) (char= ch #\$)))

(defun java-alphanumericp (ch)
  (or (alphanumericp ch) (char= ch #\_) (char= ch #\$)))


(defun read-java-method-name (s)
  "Returns a string that names a java method"
  ;; I decided to implement my own reading function
  ;; instead of using (read) with :PRESERVE on the readtable
  ;; in order to follow more closely Java's syntax on method names.
  (let ((ch (peek-char nil s t nil t))
	(i 0)
	buf
	last-dot
	next-to-last-dot
	)
    (when (java-alpha-char-p ch)
      (setq buf (make-array '(50) :element-type 'character
			    :adjustable t
			    :fill-pointer 0))
      (do ((ch (read-char s t nil t) (read-char s t nil t))
	   )
	  ((not (or (java-alphanumericp ch) (char= ch #\.))) (unread-char ch s))
	(vector-push-extend ch buf 50)
	(when (char= ch #\.)
	  (setq next-to-last-dot last-dot
		last-dot i)
	  )
	(incf i)
	;;(print ch)
	)
      (let (class-name field-name method-name)
	(cond ((and (null last-dot) (null next-to-last-dot))
	       ;; this is a basic instance method call.
	       (setq method-name buf class-name nil field-name nil)
	       )
	      ((null next-to-last-dot)
	       ;; this is a static method call on an on-demand import class name.
	       (setq method-name (subseq buf (1+ last-dot)))
	       (setq class-name (subseq buf 0 last-dot))
	       (setq field-name nil)
	       )
	      (t
	       (setq method-name (subseq buf (1+ last-dot)))
	       (setq field-name (subseq buf (1+ next-to-last-dot) last-dot))
	       (setq class-name (subseq buf 0 next-to-last-dot))
	       )
	      )
	(values buf class-name field-name method-name)
	)
      )
    )
  )

(defun read-java-method-call (stream sub-char num-arg)
  (declare (ignore num-arg sub-char))
  (multiple-value-bind (whole class-name field-name method-name)
	(read-java-method-name stream)
    (declare  (ignore whole))
    (cond (field-name
	   ;; instance method on a static field or static method.
	   `(lambda (&rest args)
	      (let ((class (find-java-class ,class-name)))
		(cond (class
		       ;; This check is probably redundant and incomplete.
		       ;; Maybe it should be removed. The call to jfield-static
		       ;; will do the work anyway.
		       (unless (find-java-static-field class ,field-name)
			 (error "Field ~S does not exist on Java class ~S.~%"
				,field-name ,class-name)
			 )
		       (apply #'jmethod-instance
			      ;;(jfield ,class-name ,field-name)
			      (jfield-static ,class-name ,field-name)
			      ,method-name
			      args)
		       )
		      (t
		       (setq class
			     (find-java-class
			      ,(setq class-name (str+ class-name "." field-name))))
		       (unless class
			 (error "Unknown Java class ~S.~%" ,class-name))
		       (apply #'jmethod-static
			      class
			      ,method-name
			      args)
		       )
		      )
		)
	      )
	   )
	  (class-name
	   ;; static method on on-demand import class.
	   `(lambda (&rest args)
	      (apply #'jmethod-static
		     (find-java-class ,class-name) ,method-name args))
	   )
	  (method-name
	   ;; basic instance method.
	   `(lambda (java-object &rest args)
	      (apply #'jmethod-instance java-object ,method-name args))
	   )
	  (t
	   ;; this case is not really possible since "method-name"
	   ;; will always be at least an empty string, which is not nil.
	   (warn "Empty java method invocation! Generating empty function.")
	   `(lambda (&rest args)
	      (declare (ignore args))
	      nil)
	   )
	  )

    )
  )

(defun read-java-field-name (s)
  "Returns a string that names a java method"
  ;; I decided to implement my own reading function
  ;; instead of using (read) with :PRESERVE on the readtable
  ;; in order to follow more closely Java's syntax on method names.
  (let ((ch (peek-char nil s t nil t))
	(i 0)
	buf
	last-dot
	)
    (when (java-alpha-char-p ch)
      (setq buf (make-array '(50) :element-type 'character
			    :adjustable t
			    :fill-pointer 0))
      (do ((ch (read-char s t nil t) (read-char s t nil t))
	   )
	  ((not (or (java-alphanumericp ch) (char= ch #\.))) (unread-char ch s))
	(vector-push-extend ch buf 50)
	(when (char= ch #\.)
	  (setq last-dot i)
	  )
	(incf i)
	;;(print ch)
	)
      (let (class-name field-name)
	(cond ((null last-dot)
	       ;; this is a basic instance field access.
	       (setq field-name buf class-name nil)
	       )
	      (t
	       ;; this is a static field access on a Java class name.
	       (setq field-name (subseq buf (1+ last-dot)))
	       (setq class-name (subseq buf 0 last-dot))
	       )
	      )
	(values buf class-name field-name)
	)
      )
    )
  )

(defun read-java-field-get (stream sub-char num-arg)
  (declare (ignore num-arg sub-char))
  (multiple-value-bind (whole class-name field-name)
	(read-java-field-name stream)
    (declare  (ignore whole))
    (cond (class-name
	   (cond ((string= field-name "class")
		  `(java-class-real (find-java-class ,class-name))
		  )
		 (t
		  ;; static field on on-demand import class.
		  `(jfield-static ,class-name ,field-name)
		  )
		 )
	   )
	  (field-name
	   ;; basic instance field.
	   `(lambda (java-object)
	      (jfield-instance java-object ,field-name))
	   )
	  (t
	   ;; this case is not really possible since "field-name"
	   ;; will always be at least an empty string, which is not nil.
	   (warn "Empty java method invocation! Generating empty function.")
	   `(lambda (&rest args)
	      (declare (ignore args))
	      nil)
	   )
	  )

    )
  )

(defun read-java-field-set (stream sub-char num-arg)
  (declare (ignore num-arg sub-char))
  (multiple-value-bind (whole class-name field-name)
      (read-java-field-name stream)
    (declare  (ignore whole))
    (cond (class-name
	   ;; static field on on-demand import class.
	   `(lambda (value)
	      (jfield-static-set ,class-name ,field-name value))
	   )
	  (field-name
	   ;; basic instance field.
	   `(lambda (java-object value)
	      (jfield-instance-set java-object ,field-name value))
	   )
	  (t
	   ;; this case is not really possible since "field-name"
	   ;; will always be at least an empty string, which is not nil.
	   (warn "Empty java method invocation! Generating empty function.")
	   `(lambda (&rest args)
	      (declare (ignore args))
	      nil)
	   )
	  )
    )
  )

(defvar method-dispatch-macro-sub-character #\_)
(defvar field-getter-dispatch-macro-sub-character #\?)
(defvar field-setter-dispatch-macro-sub-character #\!)

(defun setup-readtable-for-cl+j (&optional (readtable *readtable*))
  (flet ((setup-reader (sub-char reader)
	   (set-dispatch-macro-character #\# sub-char reader readtable)
	   )
	 )
    (setup-reader method-dispatch-macro-sub-character #'read-java-method-call)
    (setup-reader field-getter-dispatch-macro-sub-character #'read-java-field-get)
    (setup-reader field-setter-dispatch-macro-sub-character #'read-java-field-set)
    )
  readtable
  )

(defvar +preserved-readtable+ nil)

(unless +preserved-readtable+
  (setq +preserved-readtable+ (copy-readtable *readtable*)))

(defvar *cl+j-readtable* (setup-readtable-for-cl+j (copy-readtable)))

#-ccl (setup-readtable-for-cl+j)

#+ccl (let ((method-dispatch-macro-sub-character #\]))
	;;(break "about to setup readtable for CL+J.")
	(setup-readtable-for-cl+j))

#+ccl (defvar *cl+j-readtable-for-ccl* (copy-readtable))

;;;
;;;

(defvar +java-init-lock+ (bt:make-lock "CL+J initialization"))
(defvar +java-init-done+ nil)

(defun java-init (&key force)
  (declare (ignorable force))

  (when +java-init-done+ (return-from java-init nil))

  (bt:with-lock-held (+java-init-lock+)

    (when +java-init-done+  ;; somebody else had beaten us to it.
      (return-from java-init nil))

    #+(and sbcl linux)
    (unless (or force jni::sbcl-can-use-initial-thread)
      (when (jni::in-initial-thread-p)
	(format t "~%****~%In SBCL on Linux, the 'initial thread' cannot be used to~%initialize or interact with the Java virtual machine.~%As a work around you could call (spawn-repl) first!~%****~%")
	(return-from java-init nil)
	)
      )

    (unless (java-vm-initialized) ;;jni::*jvm-foreign-ptr*
      (load-libjvm)
      (create-java-vm)
      )
    (init-java-primitive-types)
    (init-java-basic-classes)
    (init-java-exceptions)

    (register-pending-java-natives)
    (jtol-init)
    (setq +java-init-done+ t)
    )
  )

(defun java-destroy ()
  (destroy-java-vm)
  )



;;;
;;;


(defun java-type-of (ref)
  (with-pinned-values (ref)
    (when (jref-p ref) (setq ref (jref-it ref)))
    (with-jvm-local-frame (4)
      (java-string-to-lisp
       (jmethod-object-0
	(GetObjectClass ref)
	"toString" "()Ljava/lang/String;"))
      )
    )
  )

(export 'java-type-of)
