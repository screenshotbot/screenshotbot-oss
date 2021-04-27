(defpackage :screenshotbot/java/java
  (:use :cl)
  (:export :*bfalse*
   :*btrue*
           :safe-jvref
   :invoke
   :java-equals
           :classp
           :jclass-of
           :read-java-field
           :new-instance
   :jclass-of!
           :array->list)
  #+lispworks
  (:import-from :lw-ji
   :find-java-class
   :java-objects-eq
   :java-array-length)
  #+ccl
  (:import-from :cl+j))
(in-package :screenshotbot/java/java)

#+lispworks
(require "java-interface")

(defvar *bfalse*)
(defvar *btrue*)

#+ccl
(declaim (optimize (speed 0) (debug 3)))

#+ccl
(defun java-objects-eq (x y)
  (cond
    ((not (eql (type-of x) (type-of y)))
     nil)
    ((typep x 'cl+j::java-class)
     (java-objects-eq (cl+j::java-class-real x)
                      (cl+j::java-class-real y)))
    (t
     (cl+j:jeq (fix-arg-for-jmethod-invoke x) (fix-arg-for-jmethod-invoke y)))))

#+ccl
(defun find-java-class (name)
  (case name
    (:int
     cl+j::java-int-prim-type)
    (otherwise
     ;; this doesn't match the find-java-class on Lispworks, so we're
     ;; assuming we're only using this internally within this java
     ;; package.
     (cl+j:find-java-class name))))

(defun %invoke-static (class method &rest args)
  (if (symbolp method)
      (setf method (symbol-name method)))
  (apply 'send_static_method class method args))

(defun send_static_method (klass method-name &rest args)
  (init-booleans)
  (let ((ret
         (%send_static_method klass method-name (%make-java-array args))))
    (unwrap-primitive ret)))

(defun jobject-p (obj)
  (#+lispworks lw-ji:jobject-p
   #+ccl cl+j::jobjectp
   #- (or ccl lispworks) (lambda (x) (error "unsupported"))
   obj))

#+ccl
(defun safe-jtol (x)
  (cond
    ((jobject-of-class-p x "java.lang.Integer")
     (cl+j:jtol (cl+j:jmethod x "intValue")))
    ((jobject-of-class-p x "java.lang.String")
     (cl+j::java-string-to-lisp (cl+j::jref-it x)))
    ((jobject-p x) x)
    ((stringp x) (cl+j:jtol x))
    ((eql cl+j:jnull x)
     nil)
    (t
     x)))

(defun unwrap-primitive (ret)
  (#+lispworks identity
   #+ccl safe-jtol
   (cond
     ((not (jobject-p ret))
      ret)
     ((jobject-of-class-p ret "com.tdrhq.PrimitiveWrapper")
      (let ((type (pm-get-type ret)))
        (case type
          (1 (pm-as-boolean ret))
          (2 (pm-as-character ret))
          (otherwise (error "unimplemented")))))
     (t ret))))

#+ccl
(defun fix-arg-for-jmethod-invoke (arg)
  (format t "fixing arg: ~S~%" arg)
  (finish-output t)
  (cond
    ((stringp arg)
     (cl+j:jstring  arg))
    ((numberp arg)
     ;; I think the java side handles converting this to Integer if
     ;; needed when used with reflection.
     (cl+j:jnew "java.lang.Long" (cl+j:jprim-long arg)))
    ((null arg)
     cl+j:jnull)
    (t
     (assert (jobject-p arg))
     arg)))

(defun fix-return-type (val return-type)
  (case return-type
    (:boolean
     (> val 0))
    (t
     val)))

#+ccl
(defmacro define-single-java-caller (class-name fn-name name &key signatures staticp
                                                               return-type)
  (declare (ignore signatures))
  `(defun ,fn-name (&rest args)
     (declare (optimize (speed 0) (debug 3)))
     (#+ccl fix-return-type
      #+lispworks identity
      (#+ccl safe-jtol
       #+lispworks identity
       ,(if staticp
            `(apply 'cl+j::jmethod-static (cl+j::Find-java-class ,class-name)
                     ,name (mapcar 'fix-arg-for-jmethod-invoke args))
            `(apply 'cl+j::jmethod-instance (fix-arg-for-jmethod-invoke (car args)) ,name (mapcar 'fix-arg-for-jmethod-invoke (cdr args)))))
      ,return-type)))

#+ccl
(defmacro define-java-callers (class-name &rest args)
  (declare (ignore class-name))
  `(progn
     ,@ (loop for arg in args
              collect
              `(define-single-java-caller ,class-name ,@arg))))

#+lispworks
(defmacro define-java-callers (class-name &rest args)
  `(lw-ji:define-java-callers ,class-name
     ,@ (loop for arg in args
              collect
              (destructuring-bind (name real-name &key signatures staticp return-type)
                  arg
                (declare (ignore signatures staticp))
                `(,name ,real-name)))))

(define-java-callers "java.lang.ClassLoader"
    (load-class "loadClass" :signatures ("java.lang.String")))

(define-java-callers "com.tdrhq.PrimitiveWrapper"
  (pm-get-type "getType")
  (pm-as-boolean "asBoolean")
  (pm-as-character "asCharacter"))

(define-java-callers "java.lang.Object"
  (java-get-class "getClass"))

(define-java-callers "java.lang.Class"
  (java-class-is-array "isArray"))



(defun jvref (arr i)
  #+lispworks
  (lw-ji:jvref arr i)
  #+ccl
  (safe-jtol (cl+j:jaref arr i)))

(defun (setf jvref) (value arr i)
  #+lispworks
  (setf (lw-ji:jvref arr i)
        value)
  #+ccl
  (cl+j::jaref-set arr (fix-arg-for-jmethod-invoke value) i)
  #- (or ccl lispworks)
  (error "unimplemented"))

(defun safe-jvref (arr i)
  (let ((ret (jvref arr i)))
    ret))

(defun %make-java-array (args)
  (format t "Creating array out of ~S~%" args)
  (let (#+ccl (args (mapcar 'fix-arg-for-jmethod-invoke args)))
   (let ((ret (#+lispworks lw-ji:make-java-array
               #+ccl cl+j::jnew[]
               "java.lang.Object" (length args))))
     (loop for i from 0 to 100000
           for x in args
           do
              (setf (jvref ret i) x))
     (assert ret)
     ret)))

(define-java-callers "com.tdrhq.SimpleNativeLibrary"
    (%send_static_method "send_static_method_wrapped"
                         :signatures ("java.lang.Class" "java.lang.String" "[java.lang.Object")
                         :staticp t)
    (%send_method "send_method_wrapped"
                  :signatures ("java.lang.Object" "java.lang.String" "[java.lang.Object")
                  :staticp t)
    (%%new-instance "newInstance_wrapped"
                    :signatures ("java.lang.Class" "[java.lang.Object")
                    :staticp t)
    (%%read-java-field "readField" :staticp t)
  (%%read-static-field "readStaticField" :staticp t)
  (%%write-field "writeField" :staticp t)
  (%%write-static-field "writeStaticField" :staticp t)
    (%get-logger "getLogger" :staticp t))

(defun %new-instance (klass &rest args)
  (format t "Creating new instance ~S, ~S~%" klass args)
  (unwrap-primitive (%%new-instance klass (%make-java-array args))))


(defun init-booleans ()
  (unless (boundp '*btrue*)
    (format t "init-booleans~%")
    (setf *btrue* (read-java-field (intern "com.tdrhq.SimpleNativeLibrary" :keyword) "btrue"))
    (setf *bfalse* (read-java-field (intern "com.tdrhq.SimpleNativeLibrary" :keyword) "bfalse"))))

(defun send_method (obj method-name &rest args)
  (init-booleans)
  (unwrap-primitive (%send_method obj method-name (%make-java-array args))))

#+lispworks
(defun invoke (obj method &rest args)
  (let ((method (string method)))
   (let ((is-static (symbolp obj)))
     (if is-static
         (let ((class (jclass-of! obj)))
           (apply #'%invoke-static class method args))
         (apply #'invoke-instance obj method args)))))

#+ccl
(defun invoke (obj method &rest args)
  (safe-jtol
   (let ((args (mapcar 'fix-arg-for-jmethod-invoke args)))
     (cond
       ((symbolp obj)
        (apply 'cl+j::jmethod-static (jclass-of! obj) method args))
       (t
        (apply 'cl+j::jmethod-instance (fix-arg-for-jmethod-invoke obj)
                method args))))))

#+lispworks
(defun read-java-field (obj field)
  (format t "read-java-field: ~S, ~S~%" obj field)
  (finish-output t)
  (let ((field (string field)))
   (let ((staticp (symbolp obj)))
     (if staticp
         (let ((class (jclass-of! obj)))
           (format t "class is ~S, ~S~%" class field)
           (finish-output t)
           (%%read-static-field class field))
         (%%read-java-field obj field)))))

#+ccl
(defun read-java-field (obj field)
  (let ((field (string field)))
   (if (symbolp obj)
       (cl+j::jfield-static (string obj) field)
       (progn
         (assert (jobject-p obj))
         (cl+j::jfield-instance obj field)))))



(defun (setf read-java-field) (value obj field)
  (let ((field (string field)))
   (let ((staticp (symbolp obj)))
     (if staticp
         (let ((class (jclass-of! obj)))
           (%%write-static-field class field value))
         (%%write-field obj field value)))))

(defun invoke-instance (obj method &rest args)
  ;; always invoke the instance method on obj, even if it's a class or
  ;; symbol.
  (if (symbolp method)
      (setf method (symbol-name method)))
  (unless obj
    (error (format nil "could not apply ~a on null" method)))
  (apply #'send_method obj method args))

#+lispworks
(defun new-instance (class &rest args)
  (let ((class (jclass-of! class)))
    (assert class)
    (apply '%new-instance class args)))

#+ccl
(defun new-instance (class &rest args)
  (safe-jtol
   (apply 'cl+j:jnew (str:replace-all "/" "." (cl+j::java-class-name (jclass-of! class)))
           (mapcar 'fix-arg-for-jmethod-invoke args))))

(define-java-callers "java.lang.Object"
  (java-equals "equals" :signatures ("java.lang.Object")
               :return-type :boolean))

(defun jclass-of (sym)
  (if (classp sym)
      sym
      (when *type-locator*
        (%%locate *type-locator* sym))))

(defun jclass-of! (sym)
  (let ((ret (jclass-of sym)))
    (if ret
        ret
        (error (format nil "null class for ~a" sym)))))

(defun jobject-of-class-p (obj class-name)
  (format t "testing jobject with ~s, ~s~%" obj class-name)
  (finish-output t)
  (#+lispworks lw-ji:jobject-of-class-p
   #+ccl (lambda (obj class-name)
           (fix-return-type
            (cl+j::jmethod-instance (cl+j::java-class-real (find-java-class class-name))
                                    "isInstance"
                                    (fix-arg-for-jmethod-invoke obj))
            :boolean))
   obj class-name))

(defun classp (obj)
  (and
   (jobject-p obj)
   (jobject-of-class-p obj "java.lang.Class")))

#+ccl
(defun java-array-length (array) (cl+j::jfield-int array "length")) ;;todo: this is probably wrong

(defun array->list (arr)
  #+nil
  (lw-ji:map-java-object-array 'identity arr :convert t :write-back t)
  (when arr
   (loop for i from 0 to (- (java-array-length arr) 1)
      collect (safe-jvref arr i))))

#+lispworks
(defun safe-make-java-array (type dims)
  (let ((type (jclass-of! type)))
    (lw-ji:make-java-array (class-to-lispworks-locator type) dims)))

#+ccl
(defun safe-make-java-array (type dims)
  (cl+j::jnew[] (cl+j::java-class-name (jclass-of! type)) dims))


(defun primitive-locator (sym)
  (if (equal (string-downcase sym) (symbol-name sym))
      (setf sym (intern (string-upcase sym) "KEYWORD")))
  (flet ((unsupported () (error "unsupported type ~a" sym)))
   (unless (str:contains? "." (symbol-name sym))
     (let ((name (intern (symbol-name sym) "KEYWORD")))
       (loop for primitive in '(:byte :char :double :float :int :long :short :boolean :void)
          if (eq primitive name)
            do (return (find-java-class name)))))))

(defun fqn-locator (sym)
  (ignore-errors
   (find_class2 (symbol-name sym))))

(defclass type-locator ()
  ((cache :initform (make-hash-table :test 'equal))
   (package :initarg :package)
   (imports :initarg :imports)
   (class :initarg :class)))

(defun make-type-locator (&key package imports class)
  (unless (or (null class) (stringp class))
    (error "expected string, got ~a" class))
  (unless (or (null package) (stringp package))
    (error "expected string, got ~a" package))
  (make-instance 'type-locator
                 :package package
                 :imports imports
                 :class class))

(defun java-lang-locator (sym)
  (package-locate "java.lang" sym))

(defun package-locate (package sym)
  (find_class2 (format nil "~a.~a" package (symbol-name sym))))

(defun msubstring (start end s)
  "Too lazy to refactor this out."
  (subseq s start end))

(defun locate-from-imports (imports suffix)
  (when imports
    (let ((first (car imports))
          (rest (cdr imports)))
      (cond
        ((str:ends-with-p suffix first)
         (find_class2 first))
        (t
         (or
          (when (str:ends-with-p  ".*" first)
            (let ((prefix (msubstring 0 (+ (length first) -2) first)))
              (let ((new-name (format nil "~a~a" prefix suffix)))
                (find_class2 new-name))))
          (locate-from-imports rest suffix)))))))

(defun find_class2 (obj)
  #+ccl
  (declare (optimize (speed 0) (debug 3)))
  (or
   (primitive-locator (intern obj "KEYWORD"))
   (ignore-errors
    (handler-case
        (find-java-class (string obj))
      #+ccl
      (jni:thrown-from-java (e)
        nil)))))

(defparameter *default-locator-impl* (make-type-locator :package ""
                                                  :imports '("java.lang.*")))

(defparameter *type-locator* *default-locator-impl*)

(defun %pp (x expr)
  (log:info "Got: ~S for ~S" x expr)
  x)

(defmacro pp (x)
  `(%pp ,x ',x))

(defun %%locate-uncached (type-locator sym)
  (when type-locator
   (with-slots (package imports class) type-locator
     (or
      (pp (fqn-locator sym))
      (pp (java-lang-locator sym))
      (pp (package-locate package sym))
      (pp (locate-from-imports imports (format nil ".~a" (string sym))))))))

(defun %%locate (type-locator sym)
  #+ccl ;; haven't debugged this enough to use caching
  (%%locate-uncached type-locator sym)

  #+lispworks
  (with-slots (cache) type-locator
    (symbol-macrolet ((place (gethash sym cache)))
      (multiple-value-bind (ret present-p) place
        (if present-p
            ret
            (setf place (%%locate-uncached type-locator sym)))))))

(defun class-to-lispworks-locator (typename)
  (if (and
       (jobject-p typename)
       (jobject-of-class-p typename "java.lang.Class"))
      (setf typename (intern (invoke typename :|getName|) "KEYWORD")))
  (let ((primitives (list :int :char :boolean :short :long  :double :float :byte)))
    (dolist (prim primitives)
      (if (equal (string-upcase prim) (string-upcase typename))
          (return-from class-to-lispworks-locator prim)))
    (symbol-name typename)))

(defun java-array-p (x)
  (and
   (jobject-p x)
   (java-class-is-array (java-get-class x))))


(defun list->array (type list)
  (let ((ret (safe-make-java-array type (length list)))
        (idx 0))
    (loop for e in list
       do
         (setf (safe-jvref ret idx) e)
         (setf idx (+ 1 idx)))
    ret))

(defun safe-jvref (arr i)
  (let ((ret (jvref arr i)))
    ret))

(defun (setf safe-jvref) (val arr i)
  (setf (jvref arr i) val))
