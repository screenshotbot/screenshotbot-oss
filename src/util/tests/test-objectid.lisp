(defpackage :util.model.test-object-id
  (:use :cl
   :alexandria
        :fiveam)
  (:import-from :util
   :object-with-oid
                :oid
                :find-by-oid
   ))
(in-package :util.model.test-object-id)

(def-suite* :util.model.test-object-id)

(test simple-creation-and-finding
  (let ((obj (make-instance 'object-with-oid)))
    (is (eql obj
             (find-by-oid (oid obj))))))
