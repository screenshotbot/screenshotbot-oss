(uiop:define-package :util/make-instance-with-accessors
  (:use #:cl
        #:alexandria)
  (:export
   #:make-instance-with-accessors))
(in-package :util/make-instance-with-accessors)

(defun rename-accessor (class accessor)
  (declare (optimize debug))
  (loop for slot in (closer-mop:class-direct-slots class)
        for readers = (closer-mop:slot-definition-readers slot)
        if (member accessor readers)
          do (return (car (closer-mop:slot-definition-initargs slot)))
        finally
         (error "Could not find initarg for ~s" accessor)))

(defun make-instance-with-accessors (class &rest args)

  (let ((args (loop for (x y) on args by #'cddr
                    appending (list (rename-accessor (find-class class) x) y))))
    (apply #'make-instance class args)))
