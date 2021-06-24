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


(in-package :cl+j)


;(declaim (optimize debug))
;(declaim (optimize (debug 0) (speed 3)))

#|
(defcallback get-message-from-java 
    :void ((env :pointer) (this jobject) (msg jobject))
  ;(declare (ignore env this))
  (let ((*jenv-foreign-ptr* env))
    (declare (special *jenv-foreign-ptr*))

    (let ((message (cl+j::java-string-to-lisp msg)))
      ;;(break)
      (format t "Java object ~S sent this message: ~S.~%" this message)
      (terpri)
      )
    )
  )

(defun register-my-native-method ()
  (with-foreign-object (method-spec (jni::struct-JNINativeMethod))
    (setf (foreign-slot-value method-spec (jni::struct-JNINativeMethod) 'name)
	  "talkToLisp")
    (setf (foreign-slot-value method-spec (jni::struct-JNINativeMethod) 'signature)
	  "(Ljava/lang/String;)V")
    (setf (foreign-slot-value method-spec (jni::struct-JNINativeMethod) 'fnPtr)
	  (callback get-message-from-java))

    (RegisterNatives (FindClass "HelloLispWorld") method-spec 1)
    )
  )
|#

;;;
;;;
;;;

(defstruct java-native
  name
  class-name
  return-type
  arg-list
  callback-function-pointer
  )

(defvar *registered-java-natives* nil)

(defvar *pending-java-natives* nil)

(defvar +java-natives-lock+ (bt:make-lock "Java natives registration"))

;;;
;;;

(defmacro skip-spaces (buf head max)
  `(do () ((>= ,head ,max))
     (let ((ch (char ,buf ,head)))
       (cond ((char= ch #\space))
	     (t (return))))
     (incf ,head)))

(defun parse-java-class-or-array-type-name (java-type-name)
  (let ((i 0)
	(len (length java-type-name)) 
	base-name rank)
    (do ()
	((>= i len))
      (let ((ch (char java-type-name  i)))
	(cond ((or (alphanumericp ch) (char= ch #\.)))
	      (t (return)))
	)
      (incf i)
      )
    (setq base-name (subseq java-type-name 0 i))
    (cond ((>= i len) ;; this was not an array.
	   (return-from parse-java-class-or-array-type-name (values base-name nil))
	   )
	  (t
	   (skip-spaces java-type-name i len)
	   )
	  )
    (cond ((>= i len) ;; this is still no more array.
	   (return-from parse-java-class-or-array-type-name (values base-name nil))
	   )
	  (t
	   (unless (char= #\[ (char java-type-name i))
	     (error "Invalid Java type name: ~S~%" java-type-name))
	   (incf i)
	   (skip-spaces java-type-name i len)
	   (unless (and (< i len) (char= #\] (char java-type-name i)))
	     (error "Invalid Java type name: ~S~%" java-type-name)
	     )
	   (setq rank 1)
	   (incf i)
	   )
	  )
    (cond ((>= i len)
	   (return-from  parse-java-class-or-array-type-name (values base-name rank))
	   )
	  (t
	   (do ()
	       ((>= i len))
	     (let ((ch (char java-type-name i)))
	       (cond ((char= ch #\space))
		     (t
		      (unless (char= #\[ ch)
			(error "Invalid Java type name: ~S~%" java-type-name))
		      (incf i)
		      (skip-spaces java-type-name i len)
		      (unless (and (< i len) (char= #\] (char java-type-name i)))
			(error "Invalid Java type name: ~S~%" java-type-name)
			)
		      (incf rank)
		      )
		     )
	       )
	     (incf i)
	     )
	   
	   )
	  )
    (values base-name rank)
    )
  )

(defun map-java-type-to-descriptor (java-type-name)
  (cond ((string= "void" java-type-name)
	 "V"
	 )
	((string= "int" java-type-name)
	 "I"
	 )
	((string= "boolean" java-type-name)
	 "Z"
	 )
	((string= "byte" java-type-name)
	 "B"
	 )
	((string= "char" java-type-name)
	 "C"
	 )
	((string= "short" java-type-name)
	 "S"
	 )
	((string= "long" java-type-name)
	 "J"
	 )
	((string= "float" java-type-name)
	 "F"
	 )
	((string= "double" java-type-name)
	 "D"
	 )
	(t ;; Here we got either an object reference or an array.
	 (multiple-value-bind (base-type-name rank)
	     (parse-java-class-or-array-type-name java-type-name)
	   (cond (rank ;; this is an array type
		  (let ((base-descriptor (map-java-type-to-descriptor base-type-name))
			(descriptor-prefix ""))
		    (dotimes (i rank)
		      (setq descriptor-prefix (str+ descriptor-prefix "["))
		      )
		    (str+ descriptor-prefix base-descriptor)
		    )
		  )
		 (t ;; this is a reference type
		  (setq base-type-name 
			(java-class-name
			 (find-java-class base-type-name)))
		  (str+ "L" base-type-name ";")
		  )
		 )
	   )
	 )
	)
  )

(defun build-sig-for-callback (jreturn-type-name args)
  ;(PushLocalFrame 2)
  (let* ((args-sig "")
	 return-descriptor
	 )
    (setq jreturn-type-name (string-trim '(#\space) jreturn-type-name))
    (setq return-descriptor (map-java-type-to-descriptor jreturn-type-name))

    (dolist (arg-spec args)
      (let ((arg-jtype (car arg-spec)))
	(setq arg-jtype (string-trim '(#\space) arg-jtype))

	(setq args-sig (str+ args-sig (map-java-type-to-descriptor arg-jtype)))
	)
      )

    (str+ "(" args-sig ")" return-descriptor)
    ;(PopLocalFrame (null-pointer))
    )
  )

(defun map-to-cffi-type (java-type)
  (when (stringp java-type)
    (let ((java-type-name (string-trim '(#\space) java-type)))
      (cond ((string= "void" java-type-name)
	     :void
	     )
	    ((string= "int" java-type-name)
	     'jint
	     )
	    ((string= "boolean" java-type-name)
	     'jboolean
	     )
	    ((string= "byte" java-type-name)
	     'jbyte
	     )
	    ((string= "char" java-type-name) ;; Shouldn't it be "unsigned short" instead?
	     'jchar
	     )
	    ((string= "short" java-type-name)
	     'jshort
	     )
	    ((string= "long" java-type-name)
	     'jlong
	     )
	    ((string= "float" java-type-name)
	     'jfloat
	     )
	    ((string= "double" java-type-name)
	     'jdouble
	     )
	    ((string= "" java-type-name)
	     (error "Missing Java type specification."))
	    (t ;; Here we got either an object reference or an array.
	     (multiple-value-bind (base-name rank)
		 (parse-java-class-or-array-type-name java-type-name)
	       (declare (ignore base-name rank))
	       'jobject
	       )
	     )
	    )
      )
    )
  )

(defmacro with-condition-handler (&body body)
  `(handler-case (progn ,@body)
     (java-throwable (condi)
       (let ((j-throwable (java-throwable-thrown condi)))
	 ;;(break "About to throw Java exception from Lisp.")
	 (JNI_Throw (jref-it j-throwable))
	 ;; Do not do any other JNI calls between this point 
	 ;; and the imminent return to Java, else the JVM will blow up!
	 )
       )
     (jni::thrown-from-java (condi)
       (let ((this (jni::thrown-from-java-this condi)))
	 (JNI_Throw this) ;; it was just passing by.
	 ;; Do not do any other JNI calls between this point 
	 ;; and the imminent return to Java, else the JVM will blow up!
	 )
       )
     (condition (condi)
       (let ((j-wrapper 
	      (jnew "cl_j/LispCondition"
		    ;;(java-class-real (find-java-class "cl_j.LispCondition"))
		    (new-lisp-reference condi))))
	 ;;(break "About to throw wrapped lisp condition!")
	 (JNI_Throw (jref-it j-wrapper))
	 ;; Do not do any other JNI calls between this point 
	 ;; and the imminent return to Java, else the JVM will blow up!
	 )
       )
     )
  )

(defmacro def-java-native (java-class-name &body method-defs)
;;   (when (stringp java-class)
;;     (setq java-class (find-java-class java-class))
;;     )
  (let (callbacks)
    (bt:with-lock-held (+java-natives-lock+)
      (dolist (method-def method-defs)
	(destructuring-bind 
	      (return-type method-name 
			   (&rest args)
			   (&key (this nil this-p) 
				 (jclass nil jclass-p)
				 (super nil super-p))
			   &body body)
	    method-def
	  (declare (ignorable jclass super jclass-p super-p))
	  (let (cffi-return 
		(cffi-args (list '(this jobject) '(env :pointer))))
	    ;; The argument names "this" and "env" can too easily clash! Fix it!
	    (setq cffi-return (map-to-cffi-type return-type))
	    (dolist (arg args (setq cffi-args (nreverse cffi-args)))
	      (destructuring-bind (arg-type arg-name) arg
		(push (list arg-name (map-to-cffi-type arg-type)) cffi-args)
		)
	      )
	    (push
	     (let ((callback-name 
		    (intern (str+ "Java_" java-class-name "_" method-name))))
	       `(progn
		  (cffi:defcallback
		      ,callback-name ,cffi-return ,cffi-args
		      ,@(cond (this-p
			       `((let ((,this this))
				   (let ((jni::*jenv-foreign-ptr* env))
				     (declare (special jni::*jenv-foreign-ptr*))
				     (with-condition-handler
					 ,@body
				       )
				     )
				   ))
			       )
			      (t
			       `((declare (ignore this))
				 (let ((jni::*jenv-foreign-ptr* env))
				   (declare (special jni::*jenv-foreign-ptr*))
				   (with-condition-handler
				       ,@body
				     )
				   )
				 )
			       )
			      )
				    )
		  (eval-when (:load-toplevel :execute)
		    (let ((native-method
			   (make-java-native
			    :name ,method-name
			    :class-name ,java-class-name
			    :return-type ,return-type
			    :arg-list ',args
			    :callback-function-pointer (callback ,callback-name))))
		      (if +java-init-done+
			  (progn
			    ;;(break "About to register native.")
			    (register-java-native native-method)
			    (push native-method *registered-java-natives*))
			  (push native-method *pending-java-natives*)))
		    )
		  t
		  )
	       )
	     callbacks
	     )
	    )
	  )
	)
      )
    (cons 'progn (nreverse callbacks))
    )
  )

(defun register-java-native (this-one)
  (with-foreign-object (method-spec (jni::struct-JNINativeMethod))
    (setf (foreign-slot-value method-spec (jni::struct-JNINativeMethod) 'jni::name)
	  (java-native-name this-one))
    (setf (foreign-slot-value method-spec (jni::struct-JNINativeMethod) 'jni::signature)
	  (build-sig-for-callback (java-native-return-type this-one)
				  (java-native-arg-list this-one)))
    (setf (foreign-slot-value method-spec (jni::struct-JNINativeMethod) 'jni::fnPtr)
	  (java-native-callback-function-pointer this-one))
    (let ((real-class (java-class-real 
		       (find-java-class (java-native-class-name this-one)))))
      (RegisterNatives real-class method-spec 1)
      )
    )
  )

(defun register-pending-java-natives ()
  (bt:with-lock-held (+java-natives-lock+)
    (let ((pending (reverse *pending-java-natives*)))
      (do ()
	  ((endp pending))
	(let ((this-one (car pending)))
	  (register-java-native this-one)
	  (push this-one *registered-java-natives*)
	  (pop pending))
	)
      (setq *pending-java-natives* (reverse pending)) ;; what??? is that for unwind-protect?
      )
    )
  )



;;;
;;
;;

