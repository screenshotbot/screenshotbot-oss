(defpackage :graphs/test-dfs
  (:use #:cl
        #:graphs/dfs
        #:fiveam)
  (:local-nicknames (#:a #:alexandria)))
(in-package :graphs/test-dfs)

(util/fiveam:def-suite)

(defclass node ()
  ((id :initarg :id
       :reader node-id)
   (children :initarg :children
             :initform nil
             :accessor children)))

(defmethod print-object ((node node) stream)
  (format stream "#node<~d>" (node-id node)))

(defun make-graph (desc)
  (let* ((max (1+ (loop for x in desc maximizing
                                      (loop for y in x maximizing y))))
         (arr (make-array max :initial-contents (loop for i below max
                                                      collect (make-instance 'node :id i)))))
    (loop for (x . edges) in desc do
      (setf (children (elt arr x))
            (loop for y in edges collect (elt arr y))))
    arr))

(test preconditions
  (let ((graph (make-graph `((0 1)
                             (1 2)))))
    (is (equal (list 1)
               (mapcar #'node-id
                         (children (elt graph 0)))))))

(test simple-traversal
  (let ((graph (make-graph `((0 1)
                             (1 2)))))
    (is (equal (list 0 1 2)
               (mapcar #'node-id
                         (dfs-traversal-order (elt graph 0) :children #'children))))))

(test simple-traversal-after-order
  (let ((graph (make-graph `((0 1)
                             (1 2)))))
    (is (equal (list 2 1 0)
               (mapcar #'node-id
                         (nth-value 1
                          (dfs-traversal-order (elt graph 0) :children #'children)))))))

(test order-when-we-point-to-the-same-thing
  (let ((graph (make-graph `((0 1)
                             (1 2 0)))))
    (is (equal (list 0 1 2)
               (mapcar #'node-id
                         (dfs-traversal-order (elt graph 0) :children #'children))))))
