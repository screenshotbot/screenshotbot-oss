(defpackage :util/memory
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/memory)

(defun safe-type-of (x)
  (let ((type (type-of x)))
    (cond
      ((listp type)
       (Car type))
      (t
       type))))

(defun histogram (&key by-size)
  (let ((map (make-hash-table :test #'eql))
        (sizes (make-hash-table :test #'eql)))
    (hcl:sweep-all-objects
     (lambda (obj)
       (incf (gethash (safe-type-of obj) map 0))
       (incf (gethash (safe-type-of obj) sizes 0)
             (hcl:find-object-size obj))))
    (let ((list (loop for k being the hash-keys in map
                        using (hash-value v)
                      collect (cons k v))))
      (let ((ret (sort list #'>
                       :key (cond
                              (by-size
                               (lambda (x)
                                 (gethash (car x) sizes)))
                              (t #'cdr)))))
        (loop for (k . v) in ret
              for i below 100
              do (format t "~a ~s ~a~%" v k (gethash k sizes)))))))

(defun objects-of-type (type)
  (let ((ret))
    (hcl:sweep-all-objects
     (lambda (obj)
       (when (typep obj type)
         (push obj ret))))
    ret))

(defun weighted-random-sample (objects weight-fn)
  (let* ((total (loop for obj in objects
                      summing (funcall weight-fn obj)))
         (pt (random total)))
    (let ((curr 0))
      (dolist (obj objects)
        (incf curr (funcall weight-fn obj))
        (when (> curr pt)
          (return obj))))))


;; (histogram)
;; (random-sample:random-sample (objects-of-type 'system:tlatter) 100)
