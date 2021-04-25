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

;;(asdf:component-version (asdf::find-system :cffi))

(in-package :jni)

;(declaim (optimize debug))
;(declaim (optimize (debug 0) (speed 3)))


(defconstant JNI_VERSION_1_1 #X10001)
(defconstant JNI_VERSION_1_2 #X10002)
(defconstant JNI_VERSION_1_3 #X10003)
(defconstant JNI_VERSION_1_4 #X10004)
(defconstant JNI_VERSION_1_5 #X10005)
(defconstant JNI_VERSION_1_6 #X10006)
(defconstant JNI_VERSION_1_8 #X10008)

(export '(JNI_VERSION_1_1 JNI_VERSION_1_2 JNI_VERSION_1_3 
	  JNI_VERSION_1_4 JNI_VERSION_1_5 JNI_VERSION_1_6
          JNI_VERSION_1_8))

(defparameter *jni-version* JNI_VERSION_1_6)

;;;
;;;
;;;

#|
(eval-when (:compile-toplevel :load-toplevel)
  (defun :mkcl-version->= (major &optional minor patch)
    (let* ((si-pkg (find-package :SI))
	   (version-sym (and si-pkg (find-symbol "+MKCL-VERSION-NUMBER+" si-pkg)))
	   (req-version-num (+ (* 1000000 major) (if minor (* 10000 minor) 0) (if patch patch 0))))
      (if version-sym
	  (let ((version-num (symbol-value version-sym)))
	    (if (>= version-num req-version-num) '(:and) '(:or)))
	'(:or))
      )
    )
  )
|#

(defmacro mem-aptr! (ptr type &optional (index 0))
  (if (uiop:version< (cffi-version) "0.11.0")
      `(mem-aref ,ptr ,type ,index)
    `(mem-aptr ,ptr ,type ,index)))
(export 'mem-aptr!)


#+mkcl
(defmacro mkcl-getenv (env-var-name)
  `(,(or (find-symbol "GETENV" "SI") (find-symbol "GETENV" "MKCL")) ,env-var-name))


(eval-when (:compile-toplevel :load-toplevel :execute)

  #+cmu19 (defcfun ("getenv" posix-getenv) :string (name :string))

  #+scl (defun posix-getenv (var)
	  (cdr (assoc var ext:*environment-list* :test #'string=)))

  (defvar *libjvm-path*)

  (setq *libjvm-path*
    (let ((*jvm-path*-sym (find-symbol "*JVM-PATH*" :cl-user))
	  (*jre-home*-sym (find-symbol "*JRE-HOME*" :cl-user))
	  )
      (cond ((and *jvm-path*-sym (boundp *jvm-path*-sym))
	     (symbol-value *jvm-path*-sym))
	    ((and *jre-home*-sym (boundp *jre-home*-sym))
	     (concatenate 'string
			  (symbol-value *jre-home*-sym)

			  #+(and unix x86)
			  "/lib/i386/client/libjvm.so"
			  #+(and unix x86-64)
			  "/lib/amd64/server/libjvm.so"
			  #+windows
			  "/bin/client/jvm.dll"))
	    (t
	     (concatenate 'string 
			  (or
			   #+sbcl (SB-EXT:POSIX-GETENV "JRE_HOME")
			   #+clisp (EXT:GETENV "JRE_HOME")
			   #+cmu19 (posix-getenv "JRE_HOME")
			   #+scl (posix-getenv "JRE_HOME")
			   #+ecl (si:getenv "JRE_HOME")
			   ;;#+(and mkcl #.(mkcl-version->= 1 1)) (mk-ext:getenv "JRE_HOME")
			   ;;#+(and mkcl (not #.(mkcl-version->= 1 1))) (si:getenv "JRE_HOME")
			   #+mkcl (mkcl-getenv "JRE_HOME")
			   #+ccl (ccl:getenv "JRE_HOME")
			   "/usr/lib/jvm/jre")
			  
			  #+(and unix x86)
			  "/lib/i386/client/libjvm.so"
			  #+(and unix x86-64)
			  "/lib/amd64/server/libjvm.so"
			  #+windows
			  "/bin/client/jvm.dll"))
	    )
      )
    )
  (let ((*jre-version*-sym (find-symbol "*JRE-VERSION*" :cl-user)))
    (when (and *jre-version*-sym (boundp *jre-version*-sym))
      (setq *jni-version* (symbol-value *jre-version*-sym))
      )
    )
  ) ;; eval-when


;;;

;(define-foreign-library libpthread (t (:default "/lib/libpthread-2.5")))

;(use-foreign-library libpthread)

;;;

(defvar *libjvm-loaded* nil)

;;#+cmu19 (define-foreign-library libjvm (t *libjvm-path*))

;;#+cmu19 (use-foreign-library libjvm)

#+cmu19 (load-foreign-library *libjvm-path*)

(defun load-libjvm (&key force)
  (when *libjvm-loaded*
    #+cmu19 (return-from load-libjvm) ;; cmucl 19 does not suffer reloading gladly...

    #-cmu19 
    (if force
	(warn "Reloading Java JVM library. This may not work properly on some platforms.")
	(return-from load-libjvm))
    )
  #-cmu19
  (handler-case
      (load-foreign-library *libjvm-path*)
    #+(and windows x86)
    (cffi:load-foreign-library-error (load-error)
      (let ((msvcr71.dll (merge-pathnames "../msvcr71.dll" *libjvm-path*)))
	(cond ((probe-file msvcr71.dll) ;; this happens with Java 1.6 JRE on Win32.
	       (load-foreign-library msvcr71.dll)
	       (load-foreign-library *libjvm-path*))
	      (t (signal load-error))))))
  (setq *libjvm-loaded* t)
  )

(export 'load-libjvm)

#+clisp (load-libjvm)
#+cmu19 (load-libjvm)
#+scl (load-libjvm)
#+sbcl (load-libjvm)
#+ecl (load-libjvm) ;;??
#+mkcl (load-libjvm) ;;??
#+ccl (load-libjvm)

;;; The following 3 types are said to be Linux dependent.
(defctype jint :int)
(defctype jlong :long-long)
(defctype jbyte :char) ;; should be a :signed-char if it existed.

(defctype jboolean :unsigned-char)
(defctype jchar :unsigned-short)
(defctype jshort :short)
(defctype jfloat :float)
(defctype jdouble :double)

(export '(jint jlong jbyte jboolean jchar jshort jfloat jdouble))

(defctype jboolean* :pointer) ;; a "jboolean *"
(defctype jbyte* :pointer) ;; a "jbyte *"
(defctype jchar* :pointer) ;; a "jchar *"
(defctype jshort* :pointer) ;; a "jshort *"
(defctype jint* :pointer) ;; a "jint *"
(defctype jlong* :pointer) ;; a "jlong *"
(defctype jfloat* :pointer) ;; a "jfloat *"
(defctype jdouble* :pointer) ;; a "jdouble *"

(export '(jboolean* jbyte* jchar* jshort* jint* jlong* jfloat* jdouble*))

;(defctype jsize jint)
(defctype jsize :int)

(defctype jobject :pointer)  ;; opaque type

(defctype jclass jobject)
(defctype jthrowable jobject)
(defctype jweak jobject)
(defctype jstring jobject)
(defctype jarray jobject)

(defctype jbooleanArray jarray)
(defctype jbyteArray jarray)
(defctype jcharArray jarray)
(defctype jshortArray jarray)
(defctype jintArray jarray)
(defctype jlongArray jarray)
(defctype jfloatArray jarray)
(defctype jdoubleArray jarray)
(defctype jobjectArray jarray)

(export '(jobject jsize jclass jthrowable jweak jstring jarray
	  jbooleanArray jbyteArray jcharArray jshortArray
	  jintArray jlongArray jfloatArray jdoubleArray
	  jobjectArray))


(cffi:defcunion jvalue
	(z jboolean)
	(b jbyte)
	(c jchar)
	(s jshort)
	(i jint)
	(j jlong)
	(f jfloat)
	(d jdouble)
	(l jobject))

;;(defctype jvalue jvalue) ;; typedef ;; commented out for cffi 0.18.0
;;(defctype jvalue (:union jvalue)) ;; typedef ;; commented out for cffi 0.18.0
(defmacro union-jvalue ()
  (if (uiop:version< (cffi-version) "0.11.0") `'jvalue `'(:union jvalue)))

(defctype jvalue* :pointer) ;; A "jvalue *", usually the base of an argument vector.

(defctype jfieldID :pointer) ;; opaque type
(defctype jmethodID :pointer) ;; opaque type

(export '(jvalue z b c s i j f d l union-jvalue jvalue* jfieldID jmethodID))


;;; new in JDK 1.6

;; typedef enum _jobjectType {
;;      JNIInvalidRefType    = 0,
;;      JNILocalRefType      = 1,
;;      JNIGlobalRefType     = 2,
;;      JNIWeakGlobalRefType = 3 
;; } jobjectRefType;

(defctype jobjectRefType :int)

(defconstant JNIInvalidRefType 0)
(defconstant JNILocalRefType 1)
(defconstant JNIGlobalRefType 2)
(defconstant JNIWeakGlobalRefType 3)

(defconstant JNI_OK 0)

(defconstant JNI_NULL 'JNI_NULL)

(defconstant JNI_FALSE 0)
(defconstant JNI_TRUE 1)

(export '(JNI_OK JNI_NULL JNI_FALSE JNI_TRUE))

(defconstant JNI_ERR -1)
(defconstant JNI_EDETACHED -2)
(defconstant JNI_EVERSION -3)
(defconstant JNI_ENOMEM -4)
(defconstant JNI_EEXIST -5)
(defconstant JNI_EINVAL -6)
(defconstant JNI_COMMIT 1)
(defconstant JNI_ABORT 2)

(defcstruct JNINativeMethod
  (name :string)
  (signature :string)
  (fnPtr :pointer))

;(defctype JNINativeMethod (:struct JNINativeMethod)) ;; cffi 0.18.0
(defmacro struct-JNINativeMethod ()
  (if (uiop:version< (cffi-version) "0.11.0") `'JNINativeMethod `'(:struct JNINativeMethod)))

(export '(JNINativeMethod struct-JNINativeMethod))

(defctype JNINativeMethod* :pointer)

;;;
;;; In C, JavaVM and JNIEnv are just a pointer
;;; to a fake C++ vtable.
(defctype JavaVM :pointer) ;; "struct JNIInvokeInterface *"
(defctype JNIEnv :pointer) ;; "struct JNINativeInterface *"

(defctype JavaVM* :pointer) ;; "JavaVM *"
(defctype JNIEnv* :pointer) ;; "JNIEnv *"

;;;
;;;
;;; Here comes the JNIEnv vtable definition
;;;


(defvtable JNINativeInterface_ (jenv) (:export-all t)
  reserved0
  reserved1
  reserved2

  reserved3
  (GetVersion jint ())

  (DefineClass jclass ((name :string) (loader jobject) (buf :pointer) (len jsize))
    :not-ok JNI_NULL
    :throws (ClassFormatError 
	     NoClassDefFoundError 
	     ClassCircularityError
	     OutOfMemoryError))
  (FindClass jclass ((name :string))
	     :not-ok JNI_NULL
	     :throws (ClassFormatError 
		      NoClassDefFoundError 
		      ClassCircularityError
		      OutOfMemoryError
		      ExceptionInInitializerError))

  (FromReflectedMethod jmethodID ((method jobject))
		       :not-ok JNI_NULL
		       :throws OutOfMemoryError)
  (FromReflectedField jfieldID ((field jobject))
		      :not-ok JNI_NULL
		      :throws OutOfMemoryError)

  (ToReflectedMethod 
   jobject ((cls jclass) (methodID jmethodID) (isStatic jboolean))
   :not-ok JNI_NULL
   :throws OutOfMemoryError)

  (GetSuperClass jclass ((sub jclass)))
  (IsAssignableFrom jboolean ((sub jclass) (sup jclass)))

  (ToReflectedField 
   jobject ((cls jclass) (fieldID jfieldID) (isStatic jboolean))
   :not-ok JNI_NULL
   :throws OutOfMemoryError)

  ;; The following two (JNI_Throw and JNI_ThrowNew) are dangerous
  ;; little things to use.  Once you have called any of them
  ;; you have to run to the closest return to Java point and
  ;; not call most of the other JNI functions on your way,
  ;; otherwise you run the risk of seeing the JVM blow up right under you.
  ;; Please read the fine print in the JNI specification about
  ;; pending exceptions and (the few) JNI functions safe to call is such situation.
  (JNI_Throw jint ((obj jthrowable)) :ok JNI_OK)
  (JNI_ThrowNew jint ((clazz jclass) (msg :string)) :ok JNI_OK)

  ;; The recommendation above about reading the fine print
  ;; on pending exceptions applies also to the following three.
  (ExceptionOccurred jthrowable ())
  (ExceptionDescribe :void ())
  (ExceptionClear :void ())

  (FatalError :void ((msg :string)))

  (PushLocalFrame jint ((capacity jint)) :ok JNI_OK :throws OutOfMemoryError)
  (PopLocalFrame jobject ((result jobject)))

  (NewGlobalRef jobject ((lobj jobject)))
  (DeleteGlobalRef :void ((gref jobject)))
  (DeleteLocalRef :void ((obj jobject)))
  (IsSameObject jboolean ((obj1 jobject) (obj2 jobject)))
  (NewLocalRef jobject ((ref jobject)))
  (EnsureLocalCapacity jint ((capacity jint))
		       :throws OutOfMemoryError :ok JNI_OK)

  (AllocObject jobject ((clazz jclass))
	       :not-ok JNI_NULL
	       :throws (InstantiationException OutOfMemoryError))
  NewObject ;; stub
  NewObjectV ;; stub
  (NewObjectA 
   jobject ((clazz jclass) (methodID jmethodID) (args jvalue*))
   :not-ok JNI_NULL
;;   :throws (InstantiationException OutOfMemoryError Exception))
   :throws (InstantiationException OutOfMemoryError)
   :throws-any-exception t)

  (GetObjectClass jclass ((obj jobject)))
  (IsInstanceOf jboolean ((obj jobject) (clazz jclass)))

  (GetMethodID jmethodID ((clazz jclass) (name :string) (sig :string))
	       :not-ok JNI_NULL
	       :throws (NoSuchMethodError
			ExceptionInInitializerError
			OutOfMemoryError))
  
  CallObjectMethod ;; stub
  CallObjectMethodV ;; stub
  (CallObjectMethodA 
   jobject ((obj jobject) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallBooleanMethod ;; stub
  CallBooleanMethodV ;; stub
  (CallBooleanMethodA 
   jBoolean ((obj jobject) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallByteMethod ;; stub
  CallByteMethodV ;; stub
  (CallByteMethodA 
   jbyte ((obj jobject) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallCharMethod ;; stub
  CallCharMethodV ;; stub
  (CallCharMethodA 
   jchar ((obj jobject) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallShortMethod ;; stub
  CallShortMethodV ;; stub
  (CallShortMethodA 
   jshort ((obj jobject) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallIntMethod ;; stub
  CallIntMethodV ;; stub
  (CallIntMethodA 
   jint ((obj jobject) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallLongMethod ;; stub
  CallLongMethodV ;; stub
  (CallLongMethodA 
   jlong ((obj jobject) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallFloatMethod ;; stub
  CallFloatMethodV ;; stub
  (CallFloatMethodA 
   jfloat ((obj jobject) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallDoubleMethod ;; stub
  CallDoubleMethodV ;; stub
  (CallDoubleMethodA 
   jdouble ((obj jobject) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallVoidMethod ;; stub
  CallVoidMethodV ;; stub
  (CallVoidMethodA 
   :void ((obj jobject) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallNonvirtualObjectMethod ;; stub
  CallNonvirtualObjectMethodV ;; stub
  (CallNonvirtualObjectMethodA 
   jobject 
   ((obj jobject) (clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallNonvirtualBooleanMethod ;; stub
  CallNonvirtualBooleanMethodV ;; stub
  (CallNonvirtualBooleanMethodA 
   jboolean 
   ((obj jobject) (clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallNonvirtualByteMethod ;; stub
  CallNonvirtualByteMethodV ;; stub
  (CallNonvirtualByteMethodA 
   jbyte 
   ((obj jobject) (clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallNonvirtualCharMethod ;; stub
  CallNonvirtualCharMethodV ;; stub
  (CallNonvirtualCharMethodA 
   jchar 
   ((obj jobject) (clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallNonvirtualShortMethod ;; stub
  CallNonvirtualShortMethodV ;; stub
  (CallNonvirtualShortMethodA 
   jshort 
   ((obj jobject) (clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallNonvirtualIntMethod ;; stub
  CallNonvirtualIntMethodV ;; stub
  (CallNonvirtualIntMethodA 
   jint 
   ((obj jobject) (clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallNonvirtualLongMethod ;; stub
  CallNonvirtualLongMethodV ;; stub
  (CallNonvirtualLongMethodA 
   jlong 
   ((obj jobject) (clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallNonvirtualFloatMethod ;; stub
  CallNonvirtualFloatMethodV ;; stub
  (CallNonvirtualFloatMethodA 
   jfloat 
   ((obj jobject) (clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallNonvirtualDoubleMethod ;; stub
  CallNonvirtualDoubleMethodV ;; stub
  (CallNonvirtualDoubleMethodA 
   jdouble 
   ((obj jobject) (clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallNonvirtualVoidMethod ;; stub
  CallNonvirtualVoidMethodV ;; stub
  (CallNonvirtualVoidMethodA 
   :void 
   ((obj jobject) (clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  (GetFieldID jfieldID ((clazz jclass) (name :string) (sig :string))
	      :not-ok JNI_NULL
	      :throws (NoSuchFieldError ExceptionInInitializerError OutOfMemoryError))

  (GetObjectField jobject ((obj jobject) (fieldID jfieldID)))
  (GetBooleanField jboolean ((obj jobject) (fieldID jfieldID)))
  (GetByteField jbyte ((obj jobject) (fieldID jfieldID)))
  (GetCharField jchar ((obj jobject) (fieldID jfieldID)))
  (GetShortField jshort ((obj jobject) (fieldID jfieldID)))
  (GetIntField jint ((obj jobject) (fieldID jfieldID)))
  (GetLongField jlong ((obj jobject) (fieldID jfieldID)))
  (GetFloatField jfloat ((obj jobject) (fieldID jfieldID)))
  (GetDoubleField jdouble ((obj jobject) (fieldID jfieldID)))

  (SetObjectField 
   :void ((obj jobject) (fieldID jfieldID) (val jobject)))
  (SetBooleanField
   :void ((obj jobject) (fieldID jfieldID) (val jboolean)))
  (SetByteField
   :void ((obj jobject) (fieldID jfieldID) (val jbyte)))
  (SetCharField
   :void ((obj jobject) (fieldID jfieldID) (val jchar)))
  (SetShortField
   :void ((obj jobject) (fieldID jfieldID) (val jshort)))
  (SetIntField
   :void ((obj jobject) (fieldID jfieldID) (val jint)))
  (SetLongField
   :void ((obj jobject) (fieldID jfieldID) (val jlong)))
  (SetFloatField
   :void ((obj jobject) (fieldID jfieldID) (val jfloat)))
  (SetDoubleField
   :void ((obj jobject) (fieldID jfieldID) (val jdouble)))

  (GetStaticMethodID
   jmethodID ((clazz jclass) (name :string) (sig :string))
   :not-ok JNI_NULL
   :throws (NoSuchMethodError ExceptionInInitializerError OutOfMemoryError))
  
  CallStaticObjectMethod ;; stub
  CallStaticObjectMethodV ;; stub
  (CallStaticObjectMethodA 
   jobject 
   ((clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallStaticBooleanMethod ;; stub
  CallStaticBooleanMethodV ;; stub
  (CallStaticBooleanMethodA 
   jboolean 
   ((clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallStaticByteMethod ;; stub
  CallStaticByteMethodV ;; stub
  (CallStaticByteMethodA 
   jbyte 
   ((clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallStaticCharMethod ;; stub
  CallStaticCharMethodV ;; stub
  (CallStaticCharMethodA 
   jchar 
   ((clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallStaticShortMethod ;; stub
  CallStaticShortMethodV ;; stub
  (CallStaticShortMethodA 
   jshort 
   ((clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallStaticIntMethod ;; stub
  CallStaticIntMethodV ;; stub
  (CallStaticIntMethodA 
   jint 
   ((clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallStaticLongMethod ;; stub
  CallStaticLongMethodV ;; stub
  (CallStaticLongMethodA 
   jlong 
   ((clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallStaticFloatMethod ;; stub
  CallStaticFloatMethodV ;; stub
  (CallStaticFloatMethodA 
   jfloat 
   ((clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallStaticDoubleMethod ;; stub
  CallStaticDoubleMethodV ;; stub
  (CallStaticDoubleMethodA 
   jdouble 
   ((clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  CallStaticVoidMethod ;; stub
  CallStaticVoidMethodV ;; stub
  (CallStaticVoidMethodA 
   :void 
   ((clazz jclass) (methodID jmethodID) (args jvalue*))
   :throws-any-exception t)
  
  (GetStaticFieldID jfieldID ((clazz jclass) (name :string) (sig :string))
		    :not-ok JNI_NULL
		    :throws (NoSuchFieldError
			     ExceptionInInitializerError
			     OutOfMemoryError))

  (GetStaticObjectField jobject ((clazz jclass) (fieldID jfieldID)))
  (GetStaticBooleanField jboolean ((clazz jclass) (fieldID jfieldID)))
  (GetStaticByteField jbyte ((clazz jclass) (fieldID jfieldID)))
  (GetStaticCharField jchar ((clazz jclass) (fieldID jfieldID)))
  (GetStaticShortField jshort ((clazz jclass) (fieldID jfieldID)))
  (GetStaticIntField jint ((clazz jclass) (fieldID jfieldID)))
  (GetStaticLongField jlong ((clazz jclass) (fieldID jfieldID)))
  (GetStaticFloatField jfloat ((clazz jclass) (fieldID jfieldID)))
  (GetStaticDoubleField jdouble ((clazz jclass) (fieldID jfieldID)))

  (SetStaticObjectField 
   :void ((clazz jclass) (fieldID jfieldID) (val jobject)))
  (SetStaticBooleanField
   :void ((clazz jclass) (fieldID jfieldID) (val jboolean)))
  (SetStaticByteField
   :void ((clazz jclass) (fieldID jfieldID) (val jbyte)))
  (SetStaticCharField
   :void ((clazz jclass) (fieldID jfieldID) (val jchar)))
  (SetStaticShortField
   :void ((clazz jclass) (fieldID jfieldID) (val jshort)))
  (SetStaticIntField
   :void ((clazz jclass) (fieldID jfieldID) (val jint)))
  (SetStaticLongField
   :void ((clazz jclass) (fieldID jfieldID) (val jlong)))
  (SetStaticFloatField
   :void ((clazz jclass) (fieldID jfieldID) (val jfloat)))
  (SetStaticDoubleField
   :void ((clazz jclass) (fieldID jfieldID) (val jdouble)))

  (NewString jstring ((unicode :string) (len jsize))
	     :not-ok JNI_NULL :throws OutOfMemoryError)
  (GetStringLength jsize ((str jstring)))
  (GetStringChars jchar* ((str jstring) (isCopy jboolean*))
		  :not-ok JNI_NULL :throws OutOfMemoryError)
  (ReleaseStringChars :void ((str jstring) (chars jchar*)))

  (NewStringUTF jstring ((utf :string)) :not-ok JNI_NULL :throws OutOfMemoryError)
  (GetStringUTFLength jsize ((str jstring)))
  (GetStringUTFChars :string ((str jstring) (isCopy jboolean*))
		     :not-ok JNI_NULL
		     :throws OutOfMemoryError)
  (ReleaseStringUTFChars :void ((str jstring) (chars :string)))

  (GetArrayLength jsize ((array jarray)))

  (NewObjectArray 
   jobjectArray ((len jsize) (clazz jclass) (init jobject))
   :not-ok JNI_NULL
   :throws OutOfMemoryError)
  (GetObjectArrayElement
   jobject ((array jobjectArray) (index jsize))
   ;; :not-ok JNI_NULL  ;; why is this not said in the spec?
   :throws ArrayIndexOutOfBoundsException)
  (SetObjectArrayElement
   :void ((array jobjectArray) (index jsize) (val jobject))
   :throws (ArrayIndexOutOfBoundsException ArrayStoreException))

  (NewBooleanArray jbooleanArray ((len jsize)) :not-ok JNI_NULL :throws OutOfMemoryError)
  (NewByteArray jbyteArray ((len jsize)) :not-ok JNI_NULL :throws OutOfMemoryError)
  (NewCharArray jcharArray ((len jsize)) :not-ok JNI_NULL :throws OutOfMemoryError)
  (NewShortArray jshortArray ((len jsize)) :not-ok JNI_NULL :throws OutOfMemoryError)
  (NewIntArray jintArray ((len jsize)) :not-ok JNI_NULL :throws OutOfMemoryError)
  (NewLongArray jlongArray ((len jsize)) :not-ok JNI_NULL :throws OutOfMemoryError)
  (NewFloatArray jfloatArray ((len jsize)) :not-ok JNI_NULL :throws OutOfMemoryError)
  (NewDoubleArray jdoubleArray ((len jsize)) :not-ok JNI_NULL :throws OutOfMemoryError)

  (GetBooleanArrayElements 
   jboolean* ((array jbooleanArray) (isCopy jboolean*))
   :not-ok JNI_NULL
   :throws OutOfMemoryError)
  (GetByteArrayElements
   jbyte* ((array jbyteArray) (isCopy jboolean*))
   :not-ok JNI_NULL
   :throws OutOfMemoryError)
  (GetCharArrayElements
   jchar* ((array jcharArray) (isCopy jboolean*))
   :not-ok JNI_NULL
   :throws OutOfMemoryError)
  (GetShortArrayElements
   jshort* ((array jshortArray) (isCopy jboolean*))
   :not-ok JNI_NULL
   :throws OutOfMemoryError)
  (GetIntArrayElements
   jint* ((array jintArray) (isCopy jboolean*))
   :not-ok JNI_NULL
   :throws OutOfMemoryError)
  (GetLongArrayElements
   jlong* ((array jlongArray) (isCopy jboolean*))
   :not-ok JNI_NULL
   :throws OutOfMemoryError)
  (GetFloatArrayElements
   jfloat* ((array jfloatArray) (isCopy jboolean*))
   :not-ok JNI_NULL
   :throws OutOfMemoryError)
  (GetDoubleArrayElements
   jdouble* ((array jdoubleArray) (isCopy jboolean*))
   :not-ok JNI_NULL
   :throws OutOfMemoryError)

  (ReleaseBooleanArrayElements 
   :void ((array jbooleanArray) (elems jboolean*) (mode jint)))
  (ReleaseByteArrayElements
   :void ((array jbyteArray) (elems jbyte*) (mode jint)))
  (ReleaseCharArrayElements
   :void ((array jcharArray) (elems jchar*) (mode jint)))
  (ReleaseShortArrayElements
   :void ((array jshortArray) (elems jshort*) (mode jint)))
  (ReleaseIntArrayElements
   :void ((array jintArray) (elems jint*) (mode jint)))
  (ReleaseLongArrayElements
   :void ((array jlongArray) (elems jlong*) (mode jint)))
  (ReleaseFloatArrayElements
   :void ((array jfloatArray) (elems jfloat*) (mode jint)))
  (ReleaseDoubleArrayElements
   :void ((array jdoubleArray) (elems jdouble*) (mode jint)))

  (GetBooleanArrayRegion 
   :void ((array jbooleanArray) (start jsize) (len jsize) (buf jboolean*))
   :throws ArrayIndexOutOfBoundsException)
  (GetByteArrayRegion
   :void ((array jbyteArray) (start jsize) (len jsize) (buf jbyte*))
   :throws ArrayIndexOutOfBoundsException)
  (GetCharArrayRegion
   :void ((array jcharArray) (start jsize) (len jsize) (buf jchar*))
   :throws ArrayIndexOutOfBoundsException)
  (GetShortArrayRegion
   :void ((array jshortArray) (start jsize) (len jsize) (buf jshort*))
   :throws ArrayIndexOutOfBoundsException)
  (GetIntArrayRegion
   :void ((array jintArray) (start jsize) (len jsize) (buf jint*))
   :throws ArrayIndexOutOfBoundsException)
  (GetLongArrayRegion
   :void ((array jlongArray) (start jsize) (len jsize) (buf jlong*))
   :throws ArrayIndexOutOfBoundsException)
  (GetFloatArrayRegion
   :void ((array jfloatArray) (start jsize) (len jsize) (buf jfloat*))
   :throws ArrayIndexOutOfBoundsException)
  (GetDoubleArrayRegion
   :void ((array jdoubleArray) (start jsize) (len jsize) (buf jdouble*))
   :throws ArrayIndexOutOfBoundsException)

  (SetBooleanArrayRegion 
   :void ((array jbooleanArray) (start jsize) (len jsize) (buf jboolean*))
   :throws ArrayIndexOutOfBoundsException)
  (SetByteArrayRegion
   :void ((array jbyteArray) (start jsize) (len jsize) (buf jbyte*))
   :throws ArrayIndexOutOfBoundsException)
  (SetCharArrayRegion
   :void ((array jcharArray) (start jsize) (len jsize) (buf jchar*))
   :throws ArrayIndexOutOfBoundsException)
  (SetShortArrayRegion
   :void ((array jshortArray) (start jsize) (len jsize) (buf jshort*))
   :throws ArrayIndexOutOfBoundsException)
  (SetIntArrayRegion
   :void ((array jintArray) (start jsize) (len jsize) (buf jint*))
   :throws ArrayIndexOutOfBoundsException)
  (SetLongArrayRegion
   :void ((array jlongArray) (start jsize) (len jsize) (buf jlong*))
   :throws ArrayIndexOutOfBoundsException)
  (SetFloatArrayRegion
   :void ((array jfloatArray) (start jsize) (len jsize) (buf jfloat*))
   :throws ArrayIndexOutOfBoundsException)
  (SetDoubleArrayRegion
   :void ((array jdoubleArray) (start jsize) (len jsize) (buf jdouble*))
   :throws ArrayIndexOutOfBoundsException)

  (RegisterNatives 
   jint ((clazz jclass) (methods JNINativeMethod*) (nMethods jint))
   :throws NoSuchMethodError :ok JNI_OK)
  (UnregisterNatives jint ((clazz jclass)) :ok JNI_OK)

  (MonitorEnter jint ((obj jobject)) :throws OutOfMemoryError :ok JNI_OK)
  (MonitorExit jint ((obj jobject)) :ok JNI_OK
	       :throws (OutOfMemoryError IllegalMonitorStateException))

  (GetJavaVM jint ((vm :pointer)) ;; vm is in fact a "JavaVM **"
	     :ok JNI_OK :throws-any-exception t)

  (GetStringRegion 
   :void ((str jstring) (start jsize) (len jsize) (buf jchar*))
   :throws StringIndexOutOfBoundsException)
  (GetStringUTFRegion 
   :void ((str jstring) (start jsize) (len jsize) (buf :string))
   :throws StringIndexOutOfBoundsException)

  (GetPrimitiveArrayCritical
   :pointer ((array jarray) (isCopy jboolean*))
   :not-ok JNI_NULL
   :throws OutOfMemoryError)
  (ReleasePrimitiveArrayCritical
   :void ((array jarray) (carray :pointer) (mode jint)))

  (GetStringCritical
   jchar* ((string jstring) (isCopy jboolean*))
   :not-ok JNI_NULL  :throws OutOfMemoryError)
  (ReleaseStringCritical
   :void ((string jstring) (cstring jchar*)))

  (NewWeakGlobalRef jweak ((obj jobject)) :throws OutOfMemoryError)
  (DeleteWeakGlobalRef :void ((ref jweak)))

  (ExceptionCheck jboolean ()) ;; since at least JDK 1.2

  (NewDirectByteBuffer jobject ((address :pointer) (capacity jlong)) ;; new in JDK 1.4
		       :not-ok JNI_NULL :throws OutOfMemoryError)
  (GetDirectBufferAddress :pointer ((buf jobject)) :not-ok JNI_NULL) ;; new in JDK 1.4
  (GetDirectBufferCapacity jlong ((buf jobject)) :not-ok JNI_ERR) ;; new in JDK 1.4

  (GetObjectRefType jobjectRefType ((obj jobject))) ;; new in JDK 1.6
  )

;;;
;;;
;;;  Invocation interface
;;;


(defcstruct JavaVMOption
  (optionString :string)
  (extraInfo :pointer)) ;; "void *"

;;(defctype JavaVMOption JavaVMOption) ; cffi 0.18.0
;;(defctype JavaVMOption (:struct JavaVMOption)) ; cffi 0.18.0
(defmacro struct-JavaVMOption ()
  (if (uiop:version< (cffi-version) "0.11.0") `'JavaVMOption `'(:struct JavaVMOption)))


(defcstruct JavaVMInitArgs
  (version jint)
  (nOptions jint)
  (options :pointer) ;; "JavaVMOption *"
  (ignoreUnrecognized jboolean))

;;(defctype JavaVMInitArgs JavaVMInitArgs) ; cffi 0.18.0
;;(defctype JavaVMInitArgs (:struct JavaVMInitArgs)) ; cffi 0.18.0
(defmacro struct-JavaVMInitArgs ()
  (if (uiop:version< (cffi-version) "0.11.0") `'JavaVMInitArgs `'(:struct JavaVMInitArgs)))


(defcstruct JavaVMAttachArgs
  (version jint)
  (name :string)
  (group jobject))

;;(defctype JavaVMAttachArgs JavaVMAttachArgs) ; cffi 0.18.0
;;(defctype JavaVMAttachArgs (:struct JavaVMAttachArgs)) ; cffi 0.18.0
(defmacro struct-JavaVMAttachArgs ()
  (if (uiop:version< (cffi-version) "0.11.0") `'JavaVMAttachArgs `'(:struct JavaVMAttachArgs)))



(defcstruct JDK1_1InitArgs
  (version jint)
  (properties :pointer)
  (checkSource jint)
  (nativeStackSize jint)
  (javaStackSize jint)
  (minHeapSize jint)
  (maxHeapSize jint)
  (verifyMode jint)
  (classpath :string)
  (vprintf :pointer) ;; function pointer "jint (*)(File *, const char *, va_list)"
  (exit :pointer) ;; function pointer "void (*)(jint)"
  (abort :pointer) ;; function pointer "void (*)(void)"
  (enableClassGC jint)
  (enableVerboseGC jint)
  (disableAsyncGC jint)
  (verbose jint)
  (debugging jboolean)
  (debugPort jint)
  )

;;(defctype JDK1_1InitArgs JDK1_1InitArgs) ;; typedef of above struct ;; cffi 0.18.0
;;(defctype JDK1_1InitArgs (:struct JDK1_1InitArgs)) ;; typedef of above struct ;; cffi 0.18.0
(defmacro struct-JDK1_1InitArgs ()
  (if (uiop:version< (cffi-version) "0.11.0") `'JDK1_1InitArgs `'(:struct JDK1_1InitArgs)))


;;;
;;; Here we declare the JavaVM vtable
;;;

;;; This is bound to a "foreign pointer" to a "JavaVM"
(defvar *jvm-foreign-ptr* nil)

(defmacro java-vm-initialized ()
  `*jvm-foreign-ptr*)

(export 'java-vm-initialized)

(defun jvm ()
  (declare (special *jvm-foreign-ptr*)) ;; not really needed but confirms intent.
  ;(mem-ref *jvm-foreign-ptr* 'JavaVM*)
  (or *jvm-foreign-ptr*
      (error "~%*** Java VM is not initialized! ***~%"))
  )

(defvar *jenv-foreign-ptr* nil) ;; We need one *jenv* per thread!
;; And it should be associated with the proper *jvm*.

(defvar *JNIEnv-registry* nil)
;; Since this global a-list is not thread-protected we could
;; end up loosing some of the conses pushed on it. But since
;; this is only some kind of a cache the only damage from this
;; is some minor loss of performance. And we would loose even
;; more is we had to acquire a lock.

(defun register-current-thread (penv)
  (setq *JNIEnv-registry* 
	(acons #+sbcl sb-thread:*current-thread*
	       #+clisp t
	       #+cmu19 t
	       #+scl thread:*thread*
               #+ecl mp:*current-process*
               #+mkcl mp:*thread*
	       #+ccl ccl:*current-process*
	       penv
	       *JNIEnv-registry*))
  )

#|
(defun cleanup-dead-threads ()
  ;; Beware of race conditions!
  ;; The ice is really thin in here.
  ;;(sb-thread:thread-alive-p (caar JNI::*JNIEnv-registry*))
  )

;;(sb-ext:unschedule-timer (car (sb-ext:list-all-timers)))

#+(and sbcl (not win32))
(defconstant dead-thread-cleaner-name 
  (if (boundp 'dead-thread-cleaner-name)
      dead-thread-cleaner-name
    "CL+J dead thread cleanup"))

#+(and sbcl (not win32))
(do ((timer (find dead-thread-cleaner-name (sb-ext:list-all-timers)
		  :test #'string= :key #'sb-ext:timer-name)
	    (find dead-thread-cleaner-name (sb-ext:list-all-timers)
		  :test #'string= :key #'sb-ext:timer-name)))
    ((null timer))
    (sb-ext:unschedule-timer timer))

#+(and sbcl (not win32))
(sb-ext:schedule-timer (sb-ext:make-timer 
			#'cleanup-dead-threads 
			:name dead-thread-cleaner-name
			:thread t) 
		       120 :repeat-interval 120)
|#

(defun promote-current-thread (cell)
  (unless (eq cell (car *JNIEnv-registry*))
    ;;(setq *JNIEnv-registry* (cons cell *JNIEnv-registry*))
    (push cell *JNIEnv-registry*)
    (do ((regis *JNIEnv-registry* (cdr regis)))
	((null (cdr regis)))
      (when (eq cell (cadr regis))
	(rplacd regis (cddr regis))
	(return)
	)
      )
    )
  )

(define-condition jni-error (simple-error) ()
#|
  (:report (lambda (condition stream)
	     (format stream "~%Inside JNI-ERROR reporter.~%")
	     (apply #'format stream
		    (simple-condition-format-control condition)
		    (simple-condition-format-arguments condition))))
|#
  )

(defmethod print-object :after ((condition jni-error) stream)
  (format stream " ")
  (apply #'format stream
	 (simple-condition-format-control condition)
	 (simple-condition-format-arguments condition))
  )

#+(and sbcl linux)
(progn
  (defvar sbcl-can-use-initial-thread nil)

  (defun in-initial-thread-p ()
    (string= "initial thread" (sb-thread:thread-name sb-thread:*current-thread*))
    )

  (defmacro guard-against-initial-thread ()
    `(unless sbcl-can-use-initial-thread
       (when (in-initial-thread-p)
	 (error 'jni-error :format-control "In SBCL on Linux, the 'initial thread' cannot be attached to a Java Virtual Machine!")
	 )
       )
    )
  )

(defun jenv ()
  (declare (special *jenv-foreign-ptr*)) ;; not really needed but confirms intent.
  (cond ((null *jenv-foreign-ptr*)
	 ;; global level value
	 (let* ((cell (assoc #+sbcl sb-thread:*current-thread* 
			     #+clisp t
			     #+cmu19 t
			     #+scl thread:*thread*
			     #+ecl mp:*current-process*
			     #+mkcl mp:*thread*
			     #+ccl ccl:*current-process*
			     *JNIEnv-registry*))
		(env (cdr cell)))
	   (unless env
	     (let* ((penv (foreign-alloc 'JNIEnv*))
		    (result (GetEnv penv JNI_VERSION_1_4)))
	       (cond ((= JNI_OK result)
		       ;; we don't believe getenv.
		      #+(and sbcl linux) (guard-against-initial-thread)
		      (AttachCurrentThread penv (null-pointer))
		      ;;(register-current-thread penv)
		      (setq env (mem-ref penv 'JNIEnv*))
		      (register-current-thread env)
		      )
		     ((= JNI_EDETACHED result)
		      #+(and sbcl linux) (guard-against-initial-thread)
		      (AttachCurrentThread penv (null-pointer))
		      ;;(register-current-thread penv)
		      (setq env (mem-ref penv 'JNIEnv*))
		      (register-current-thread env)
		      )
		     ((= JNI_EVERSION result)
		      (foreign-free penv)
		      (error "JVM complained of an incompatible version number: ~S.~%"
			     JNI_VERSION_1_4)
		      )
		     (t
		      (error "GetEnv unknown result = ~S." result))
		     )
	       (foreign-free penv)
	       )
	     )
	   (when cell
	     (promote-current-thread cell))
	   ;;(mem-ref penv 'JNIEnv*)
	   env
	   )
	 )
	(t
	 ;; if we are here it is because somebody
	 ;; has bound *jenv-foreign-ptr* somewhere above
	 ;; on the stack.
	 ;;(mem-ref *jenv-foreign-ptr* 'JNIEnv*)
	 ;;(break "In (jenv).")
	 *jenv-foreign-ptr*
	 )
	)
  )

(defun handle-JNI_EDETACHED (result)
  (declare (ignore result))
  )

(defun handle-JNI_EVERSION (result)
  (declare (ignore result))
  )

(defvtable JNIInvokeInterface_ (jvm) ()
  reserved0 ;; (reserved0 :pointer)
  reserved1 ;; (reserved1 :pointer)
  reserved2 ;; (reserved2 :pointer)
  ;;(DestroyJavaVM jint () :ok JNI_OK :export t)
  (DestroyJavaVM jint () :ok JNI_OK)
  (AttachCurrentThread jint ((penv :pointer) (args :pointer)) :ok JNI_OK)
  (DetachCurrentThread jint () :ok JNI_OK)
  (GetEnv jint ((penv :pointer) (version jint))
	  :ok JNI_OK :error ((JNI_EDETACHED #'handle-JNI_EDETACHED)
			     (JNI_EVERSION #'handle-JNI_EVERSION)))
#|
  (GetEnv jint ((penv :pointer) (version jint))
	  :ok JNI_OK)
|#
  (AttachCurrentThreadDaemon jint ((penv :pointer) (args :pointer)) :ok JNI_OK)
  )

;;(export 'DestroyJavaVM)

(defvar +java-vm-destroyed+ nil)

(defun java-vm-destroyed ()
  +java-vm-destroyed+)

(defun destroy-java-vm ()
  (setq +java-vm-destroyed+ t)
  (handler-case (DestroyJavaVM)
    (simple-error (condition)
      (declare (ignorable condition))
      (error "Java Virtual Machine destruction reported a failure!~%  condition = ~S~%"
	     condition)))
;;  (unless (= JNI_OK (DestroyJavaVM))
;;    (error "Java Virtual Machine destruction reported a failure!"))
  (setq *jvm-foreign-ptr* nil)
  )

(export '(java-vm-destroyed destroy-java-vm))


#|
(defcstruct JavaVM_vtable
  (reserved0 :pointer)
  (reserved1 :pointer)
  (reserved2 :pointer)
  (DestroyJavaVM :pointer)
  (AttachCurrentThread :pointer)
  (DetachCurrentThread :pointer)
  (GetEnv :pointer)
  (AttachCurrentThreadDaemon :pointer)
  )

(defmacro struct-JavaVM_vtable ()
  (if (uiop:version< (cffi-version) "0.11.0") `'JavaVM_vtable `'(:struct JavaVM_vtable)))



(defun DestroyJavaVM (vm)
  (let ((func-ptr ;;(JavaVM_vtable-DestroyJavaVM *jvm*)))
	 (foreign-slot-value (mem-ref (jvm) :pointer)
			     (struct-JavaVM_vtable) 'DestroyJavaVM)))
    (let ((result (foreign-funcall func-ptr JavaVM* vm jint)))
      (unless (= JNI_OK result)
	(error "DestroyJavaVM failed. result = ~S." result))
      (debug-progn
	(format nil "DestroyJavaVM: result = ~S." result))
      result
      )
    )
  )

|#


;;; args is in fact a "JDK1_1InitArgs *"
(defcfun ("JNI_GetDefaultJavaVMInitArgs" JNI_GetDefaultJavaVMInitArgs)
    jint (args :pointer))
;; returns 0 if requested version is supported, otherwise returns a negative number.

(defcfun ("JNI_CreateJavaVM" JNI_CreateJavaVM)
    jint
  (pvm :pointer)   ;; "JavaVM **"
  (penv :pointer)  ;; "void **"
  (args :pointer)) ;; "void *"


;;(defvar *jvm-options*)

(defun get-jvm-options ()
;;  (setq *jvm-options*
	(let ((*jvm-options*-sym (find-symbol "*JVM-OPTIONS*" :cl-user))
	      )
	  (cond ((and *jvm-options*-sym (boundp *jvm-options*-sym))
		 (symbol-value *jvm-options*-sym))
		(t
		 #+(or sbcl clisp mkcl ecl ccl scl)
		 '("-Djava.class.path=.")
		 #+cmu19
		 '("-Djava.class.path=." "-Xmx32M")
		 )
		)
	  )
;;	)
  )


(defvar *jvm-exit-hook* nil)

(defcallback jvm-exit-handler :void ((exit-code jint))
  (debug-progn
    (format t "Inside jvm-exit-handler! exit-code = ~D~%" exit-code)
    (finish-output))

  (dolist (handler (reverse *jvm-exit-hook*)) ;; any condition handling here?
    (funcall handler exit-code))              ;;  a handler could be buggy...
  ;;(break "Inside jvm-exit-handler!~%")

  (debug-progn
    (format t "  Ran *jvm-exit-hook*!~%~%")
    (finish-output)
    (finish-output) ;; I really mean it!
    )
  )

(defvar *jvm-abort-hook* nil)

(defcallback jvm-abort-handler :void ()
  (debug-progn
    (format t "Inside jvm-abort-handler!~%")
    (finish-output))

  (dolist (handler (reverse *jvm-abort-hook*))
    (funcall handler))
  ;;(break "Inside jvm-abort-handler.")

  (debug-progn
    (format t "  Ran *jvm-abort-hook*!~%~%")
    (finish-output)
    (finish-output) ;; I really mean it!
    )
  )


(defun create-java-vm ()
  (let* ((pvm (foreign-alloc 'JavaVM*))
	 (penv (foreign-alloc 'JNIEnv*))
	 (jvm-options (get-jvm-options))
	 (nb-options (list-length jvm-options)))
    (with-foreign-object (options (struct-JavaVMOption) (+ nb-options 2)) ;; 5)
      (dotimes (i nb-options)
	(setf (foreign-slot-value (mem-aptr! options (struct-JavaVMOption) i)
				  (struct-JavaVMOption) 'optionString) (nth i jvm-options)))

      (setf (foreign-slot-value (mem-aptr! options (struct-JavaVMOption) nb-options)
				(struct-JavaVMOption) 'optionString) "exit")
      (setf (foreign-slot-value (mem-aptr! options (struct-JavaVMOption) nb-options)
				(struct-JavaVMOption) 'extraInfo)
	    (callback jvm-exit-handler))

      (setf (foreign-slot-value (mem-aptr! options (struct-JavaVMOption) (+ 1 nb-options))
				(struct-JavaVMOption) 'optionString) "abort")
      (setf (foreign-slot-value (mem-aptr! options (struct-JavaVMOption) (+ 1 nb-options))
				(struct-JavaVMOption) 'extraInfo)
	    (callback jvm-abort-handler))

      (with-foreign-object (args (struct-JavaVMInitArgs))
	(setf (foreign-slot-value args (struct-JavaVMInitArgs) 'version) JNI_VERSION_1_4)
	(setf (foreign-slot-value args (struct-JavaVMInitArgs) 'options) options)
	(setf (foreign-slot-value args (struct-JavaVMInitArgs) 'nOptions)
	      (+ 2 nb-options) ;; with exit and abort handlers.
	      ;;nb-options ;; no exit or abort handlers.
	      )
	(setf (foreign-slot-value args (struct-JavaVMInitArgs) 'ignoreUnrecognized) JNI_FALSE)
;	(setf (foreign-slot-value args (struct-JavaVMInitArgs) 'ignoreUnrecognized) JNI_TRUE)


	(let ((result (JNI_CreateJavaVM pvm penv args)))
	  (if (> JNI_OK result)
	      (if +java-vm-destroyed+
		  (error "JNI_CreateJavaVM failed. result = ~S~@
                          You most likely tried to resurrect an already destroyed VM!~%"
			 result)
		  (error "JNI_CreateJavaVM failed. result = ~S" result)
		  )
	      (progn
		;(setq *jvm-foreign-ptr* pvm)
		(setq *jvm-foreign-ptr* (mem-ref pvm 'JavaVM*)
		      +java-vm-destroyed+ nil)

		;(register-current-thread penv)
		(register-current-thread (mem-ref penv 'JNIEnv*))
		)
	      )
	  )
	)
      )
    )
  )

(defcfun ("JNI_GetCreatedJavaVMs" JNI_GetCreatedJavaVMs) jint ;; 0 on success, otherwise negative
  (arg0 :pointer)   ;; "JavaVM **"
  (arg1 jsize)      ;;
  (arg2 :pointer))  ;; "jsize *"


;(print sb-thread:*current-thread*)


(defmacro with-thread-attached-to-JavaVM ((&key (version JNI_VERSION_1_4 version-p) 
						(name nil name-p)
						(group nil group-p))
					  &body body)
  (declare (ignore version name group))
  (let ((use-attach-args (or version-p name-p group-p)))
    (cond 
      (use-attach-args
       `(let ((penv (foreign-alloc 'JNIEnv*)))
	  #+(and sbcl linux) (guard-against-initial-thread)
	  (unwind-protect
	       (with-foreign-object (args (struct-JavaVMAttachArgs) 1)
		 (setf (foreign-slot-value args (struct-JavaVMAttachArgs) 'version) version)
		 (setf (foreign-slot-value args (struct-JavaVMAttachArgs) 'name) name)
		 (setf (foreign-slot-value args (struct-JavaVMAttachArgs) 'group) group)
		 (AttachCurrentThread penv args)
		 (let ((*jenv-foreign-ptr* (mem-ref penv 'JNIEnv*)))
		   (declare (special *jenv-foreign-ptr*))
		   ,@body)
		 )
	    ;; do some clean-up here
	    (DetachCurrentThread)
	    (foreign-free penv)
	    )
	 )
       )
      (t
       `(let ((penv (foreign-alloc 'JNIEnv*)))
	  #+(and sbcl linux) (guard-against-initial-thread)
	  (unwind-protect
	       (progn
		 (AttachCurrentThread penv nil)
		 (let ((*jenv-foreign-ptr* (mem-ref penv 'JNIEnv*)))
		   (declare (special *jenv-foreign-ptr*))
		   ,@body)
		 )
	    (DetachCurrentThread)
	    (foreign-free penv)
	    )
	 )
       )
      )
    )
  )

(export 'with-thread-attached-to-JavaVM)

;;;



(defun jmethod-object-0 (this method-name sig)
  (PushLocalFrame 4)
  (let* ((this-class (GetObjectClass this))
	 (method-id (GetMethodID this-class method-name sig))
	 )
    (PopLocalFrame ;; Pops this-class
     (CallObjectMethodA this method-id (null-pointer)))
    )
  )

(defun throwable-toString (exception)
  (PushLocalFrame 2)
  (let* ((jname (jmethod-object-0 exception "toString" "()Ljava/lang/String;"))
	 (len (GetStringLength jname))
	 (utf-len (GetStringUTFLength jname))
	 (buf (foreign-alloc :char :count (1+ utf-len)))
	 )
    (GetStringUTFRegion jname 0 len buf)
    (PopLocalFrame (null-pointer)) ;; Pops jname
    (prog1 
	(cffi:foreign-string-to-lisp buf :count utf-len) ;; This is post CFFI 0.10.X
      (foreign-free buf))
    )
  )


(defun print-thrown-from-java (condition stream)
  (declare (type condition condition) (type stream stream))
  ;;(ExceptionClear) ;; Do we really need to do this here?
  (format stream "Java exception: ~S.~%"
	  (throwable-toString
	   (thrown-from-java-this condition)))
  )

;; (defun print-java-throwable (condition stream)
;;   ;;(ExceptionClear) ;; Do we really need to do this here?
;;   (format stream "Java exception: ~S.~%"
;; 	  (throwable-toString
;; 	   (java-throwable-thrown condition)))
;;   )



