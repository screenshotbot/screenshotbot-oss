(pkg:define-package :util/java/binding
    (:use #:cl
          #:alexandria)
  (:import-from #:util/java/java
                #:invoke)
  (:export
   #:bind-instance))

(defun javafy-name (name)
  (let ((name (string name)))
    (cond
      ((str:ends-with-p "-P" name)
       (str:camel-case (format nil "is-~a" (cl-ppcre:regex-replace-all "-P$" name ""))))
      (t
       (str:camel-case (format nil "get-~A" name))))))

(defun bind-instance (type java-object)
  (let ((ret (make-instance type)))
    (loop for slot in (closer-mop:class-slots (find-class type))
          do
             (let ((slot-name (closer-mop:slot-definition-name slot)))
              (setf (slot-value ret slot-name)
                    (invoke java-object
                            (javafy-name slot-name)))))
    ret))
