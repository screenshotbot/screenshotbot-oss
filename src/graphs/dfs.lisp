(defpackage :graphs/dfs
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:dfs-traversal-order
   #:dfs))
(in-package :graphs/dfs)

(defun ignore-hook (node)
  (declare (ignore node)))

(defclass frame ()
  ((node :initarg :node)
   (children :initform nil)
   (state :initform :init)
   (results :initform nil)))

(defun dfs (root-or-roots
            &key
              children
              (node-test #'eql)
              (before-hook #'ignore-hook)
              (after-hook (lambda (node results)
                            (declare (ignore node results)))))
  "A non-recursive DFS that gives the appearance of a recursive DFS

  before-hook is called before recursing into the children, and is
  called with one argument, the node being processed.

  after-hook is called after recusing into the children, and is called
  with two arguments, the node being processed, and the list of
  results from all the children. Note that the latter only makes sense
  in the case of a DAG. If there are loops, we may not be able to
  compute the return value for one of the children when the parent is
  done, and so we'll return nil. Because of how DFS works, in this
  case, we can't give much guarantees as to which call might result in
  a nil, and so it's best not to rely on results in a non-DAG."
  (let* ((seen (make-hash-table :test node-test))
         (cached-result (make-hash-table :test node-test))
         (roots (if (listp root-or-roots)
                    root-or-roots
                    (list root-or-roots)))
         (children-fn children)
         (stack nil)
         (register nil #|A register to store result of last frame|#))
    (loop for root in roots
          collect
          (progn
            (push (make-instance 'frame :node root) stack)
            (loop while stack do
              (let ((frame (car stack)))
                (with-slots (node children state results) frame
                  (ecase state
                    (:init
                     (cond
                       ((gethash node seen)
                        ;; If this is DAG we can guarantee that the
                        ;; result has been set by this point. If it's
                        ;; not a DAG, then (gethash node
                        ;; cached-result) can return nil, because it
                        ;; may actually be computed at a later
                        ;; point. e.g. in the graph A -> B -> A, we'll
                        ;; first call A, then call B. When we call A
                        ;; the next time, it's in the seen list, but
                        ;; doesn't have a cached result.
                        (setf register (gethash node cached-result))
                        (pop stack))
                       (t
                        ;; we haven't seen this yet!
                        (setf (gethash node seen) t)
                        (funcall before-hook node)
                        (setf children (funcall children-fn node))
                        (setf state :next-child))))
                    (:next-child
                     (cond
                       (children
                        (push (make-instance 'frame :node (pop children))
                              stack)
                        (setf state :wait-child))
                       (t
                        ;; we're done
                        (setf state :finally))))
                    (:wait-child
                     (push register results)
                     (setf state :next-child))
                    (:finally
                     (let ((res (funcall after-hook node (nreverse results))))
                       (setf (gethash node cached-result) res)
                       (setf register res))
                     (pop stack))))))
            register))))

(defun dfs-traversal-order (root-or-roots
                            &rest dfs-args)
  (let (before-order
        after-order)
    (apply #'dfs root-or-roots
             :before-hook (lambda (x)
                            (push x before-order))
             :after-hook (lambda (x results)
                          (push x after-order))
            dfs-args)
    (values (nreverse before-order)
            (nreverse after-order))))
