(defpackage :screenshotbot/model/transient-object
  (:use #:cl)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:util/object-id
                #:oid-array
                #:oid
                #:object-with-oid)
  (:import-from #:bknr.datastore
                #:store-object)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:with-transient-copy
   #:make-transient-clone
   #:cannot-make-transient))
(in-package :screenshotbot/model/transient-object)

(defun remove-index-args (slot-def)
  (let ((args (cdr slot-def)))
    (list*
     (car slot-def)
     (a:remove-from-plist args
                          :index-type
                          :index-initargs
                          :index-reader
                          :index-values
                          :relaxed-object-reference))))

(defmacro with-transient-copy ((transient-class parent-class)
                               &body (class-def . class-def-rest))
  (assert (not class-def-rest))
  (destructuring-bind (keyword class-name parent-classes slot-defs &rest options)
      class-def
   `(progn
      (defclass ,parent-class ()
        ())

      (defclass ,transient-class (,parent-class)
        (,@ (when (member 'object-with-oid parent-classes)
              `((oid :initarg :oid
                     :accessor oid-array)))
         (id :initarg :id
             :accessor transient-object-original-id
             :documentation "The original id, for debugging purposes")
         ,@(mapcar #'remove-index-args slot-defs)))

      (with-class-validation
        (,keyword ,class-name (,@parent-classes ,parent-class)
                  ,slot-defs
                  ,@options)))))

(define-condition cannot-make-transient (error)
  ((obj :initarg :obj)))

(defgeneric make-transient-clone (obj)
  (:documentation "Make a transient clone of a given object. The rules of creating the
 clone might vary depending on the object and business logic, so be
 sure to look at the implementation details for the object that you're
 interested in."))

(defmethod make-transient-clone :around ((self store-object))
  (let ((ret (call-next-method)))
    (setf (transient-object-original-id ret) (bknr.datastore:store-object-id self))
    ret))
