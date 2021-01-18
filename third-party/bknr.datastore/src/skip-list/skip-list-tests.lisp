(in-package :bknr.skip-list)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :unit-test))

(define-test-class skip-list-test-class)

(defmacro define-skip-list-test (name &rest body)
  `(make-instance 'skip-list-test-class
    :unit :skip-list
    :name ,name
    :body #'(lambda () ,@body)))

(define-skip-list-test "Node test"
    (let ((node (make-node 0 0 23)))
      (dotimes (i (node-level node))
	(test-equal (node-forward node i) nil))
      (dotimes (i (node-level node))
	(setf (node-forward node i) i))
      (dotimes (i (node-level node))
	(test-equal (node-forward node i) i))))

(define-skip-list-test "Skiplist test"
    (let ((sl (make-instance 'skip-list)))
      (dotimes (i 10000)
	(let ((value (random 10000))
	      (key (random 10000)))
	  (skip-list-insert sl key value)
	  (test-equal (skip-list-search sl key) value)))))

(define-skip-list-test "Hashcompare"
    (let ((sl (make-instance 'skip-list))
	  (hash (make-hash-table)))
      (dotimes (i 10000)
	(let ((value (random 10000))
	      (key (random 10000)))
	  (setf (gethash key hash) value)
	  (skip-list-insert sl key value)))
      (loop for key being the hash-key of hash
	    do (test-equal (gethash key hash)
			   (skip-list-search sl key)))))

(define-skip-list-test "Length of skiplist"
    (let ((sl (make-instance 'skip-list)))
      (dotimes (i 100)
	(skip-list-insert sl i i)
	(test-equal (skip-list-search sl i) i)
	(test-equal (skip-list-search sl (1+ i) :not-found) :not-found)
	(dotimes (j i)
	  (test-equal (skip-list-search sl j) j))
	(test-equal (skip-list-length sl) (1+ i)))
      (dotimes (i 100)
	(test-equal (skip-list-length sl) (- 100 i))
	(skip-list-delete sl i)
	(test-equal (skip-list-length sl) (- 100 (1+ i)))
	(test-equal (skip-list-search sl i :not-found) :not-found)
	(loop for j from (1+ i) to 99
	      do (test-equal (skip-list-search sl j) j)))
      (test-equal (skip-list-length sl) 0)
      (test-equal (skip-list-to-list sl) nil)))


;; (defun perf ()
;;   (let ((sl (make-instance 'skip-list)))
;;     (time (prof:with-profiling ()
;; 	    (dotimes (i 100000)
;; 	      (skip-list-insert sl i i))))
;;     (prof:show-flat-profile)
;;     (format t "~%~%")
;;     (time (prof:with-profiling ()
;; 	    (dotimes (i 100000)
;; 	      (skip-list-search sl i))))
;;     (prof:show-flat-profile)))

