(defpackage :util/memory
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/memory)


(defun histogram ()
  (let ((map (make-hash-table :test #'eql)))
    (hcl:sweep-all-objects
     (lambda (obj)
       (incf (gethash (type-of obj) map 0))))
    (let ((list (loop for k being the hash-keys in map
                        using (hash-value v)
                      collect (cons k v))))
      (let ((ret (sort list #'> :key #'cdr)))
        (loop for (k . v) in ret
              for i below 100
              do (format t "~a ~s~%" v k))))))


;; (histogram)
