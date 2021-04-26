(in-package :util)

(defun %make-oid ()
  (mongoid:oid))

(defclass object-with-oid (store-object)
  ((oid
    :initarg :oid
    :index-type bknr.indices:unique-index
    :index-initargs (:test 'equalp)
    :index-reader %find-by-oid))
  (:metaclass persistent-class))

(defmethod initialize-instance :around ((obj object-with-oid)
                                        &rest args
                                        &key oid
                                        &allow-other-keys)
  (cond
    (oid
     (call-next-method))
    (t
     (apply #'call-next-method obj :oid (%make-oid) args))))

(defclass object-with-unindexed-oid (store-object)
  ((oid
    :initform (%make-oid)))
  (:metaclass persistent-class))

(defun find-by-oid (oid &optional type)
  (let* ((oid (mongoid:oid oid))
         (obj (%find-by-oid oid)))
    (when type
      (unless (typep obj type)
        (error "Object ~s isn't of type ~s" obj type)))
    obj))

(defmethod oid ((obj object-with-oid))
  (with-slots (oid) obj
    (str:downcase (mongoid:oid-str oid))))

(defmethod oid ((obj object-with-unindexed-oid))
  (with-slots (oid) obj
    (str:downcase (mongoid:oid-str oid))))
