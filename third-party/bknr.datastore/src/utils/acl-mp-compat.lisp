(in-package :bknr.utils)

#+(not (or allegro sbcl cmu openmcl lispworks))
(error "missing port for this compiler, please provide for locking primitives for this compiler in ~A" *load-pathname*)

(defun mp-make-lock (&optional (name "Anonymous"))
  #+allegro
  (mp:make-process-lock :name name)
  #+sbcl
  (sb-thread:make-mutex :name name)
  #+(and cmu x86)
  (mp:make-lock name)
  #+(and cmu (not x86))
  (declare (ignore name))
  #+openmcl
  (ccl:make-lock name)
  #+lispworks
  (mp:make-lock :name name))

(defmacro mp-with-lock-held ((lock) &body body)
  #+allegro
  `(mp:with-process-lock (,lock)
    ,@body)
  #+sbcl
  `(sb-thread:with-mutex (,lock)
     ,@body)
  #+cmu
  `(mp:with-lock-held (,lock)
    ,@body)
  #+openmcl
  `(ccl:with-lock-grabbed (,lock)
    ,@body)
  #+lispworks
  `(mp:with-lock (,lock)
    ,@body))

(defmacro mp-with-recursive-lock-held ((lock) &body body)
  #+allegro
  `(mp:with-process-lock (,lock)
    ,@body)
  #+sbcl
  `(sb-thread:with-recursive-lock (,lock)
     ,@body)
  #+cmu
  `(mp:with-lock-held (,lock)
    ,@body)
  #+openmcl
  `(ccl:with-lock-grabbed (,lock)
    ,@body)
  #+lispworks
  `(mp:with-lock (,lock)
    ,@body))

(defun make-process (function &key name)
  (bt:make-thread function :name name))

(defun destroy-process (process)
  (bt:destroy-thread process))

(defun process-active-p (process)
  (bt:thread-alive-p process))
