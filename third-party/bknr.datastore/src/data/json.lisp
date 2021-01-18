(in-package :bknr.datastore)

(defparameter *json-ignore-slots* '(bknr.datastore::id bknr.indices::destroyed-p))

(defmacro with-json-ignore-slots ((&rest slots) &body body)
  `(let ((*json-ignore-slots* (append *json-ignore-slots* ,slots)))
     ,@body))

(defmethod yason:encode ((object store-object) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (dolist (slotdef (closer-mop:class-slots (class-of object)))
        (when (and (slot-boundp object (closer-mop:slot-definition-name slotdef))
                   (not (find (closer-mop:slot-definition-name slotdef) *json-ignore-slots*)))
          (yason:encode-object-element (string-downcase (closer-mop:slot-definition-name slotdef))
                                      (slot-value object (closer-mop:slot-definition-name slotdef))))))))
