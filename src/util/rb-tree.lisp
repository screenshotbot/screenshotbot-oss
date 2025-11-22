(defpackage :util/rb-tree
  (:use #:cl))
(in-package :util/rb-tree)


(defconstant +red+ t)
(defconstant +black+ nil)

(defclass tree-set ()
  ())

(defstruct rb-tree
  sentinel
  root
  cmp)


(defstruct node
  color
  key
  left
  right

  ;; in CLRS it's called just `p`
  parent)

(defun make-tree (&key (cmp #'<))
  (let ((rb-tree (make-rb-tree)))
    (let ((sentinel (make-node :color +black+
                               :key nil
                               :left nil
                               :right nil)))
      ;;(setf (node-parent sentinel) sentinel)
      (setf (rb-tree-sentinel rb-tree)
            sentinel)
      (setf (rb-tree-root rb-tree)
            sentinel)
      (setf (rb-tree-cmp rb-tree)
            cmp))
    rb-tree))

(defun rb-insert (Tree z)
  (%rb-insert-only Tree z)
  (rb-insert-fixup Tree z))

(defun %rb-insert-only (Tree z)
  (let* ((T.nil (rb-tree-sentinel Tree))
         (y T.nil)
         (x (rb-tree-root tree)))
    (loop while (not (eq x T.nil))
          do
             (progn
               (setf y x)
               (if (< (node-key z)
                      (node-key x))
                   (setf x (node-left x))
                   (setf x (node-right x)))))

    ;; At this point y is the left node under which we can legally put
    ;; z. But we don't know where under y this needs to be.

    (setf (node-parent z) y)
    (cond
      ((eq y T.nil)
       (setf (rb-tree-root Tree) z))
      ((< (node-key z) (node-key y))
       (setf (node-left y) z))
      (t
       (setf (node-right y) z)))
    (setf
     (node-left z) T.nil
     (node-right z) T.nil
     (node-color z) +red+)))


(defun red? (node)
  (eq (node-color node) +red+))

(defun black? (node)
  (eq (node-color node) +black+))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-symmetric-symbol (symbol)
    (let ((package (symbol-package symbol))
          (name (symbol-name symbol)))
      (setf name (str:replace-all "LEFT" "TMPL3FT" name))
      (setf name (str:replace-all "RIGHT" "LEFT" name))
      (setf name (str:replace-all "TMPL3FT" "RIGHT" name))
      (intern name package)))

  
  (defun %make-symmetric (expr)
    (cond
      ((symbolp expr)
       (make-symmetric-symbol expr))
      (t
       (loop for x in expr
             collect (%make-symmetric x))))))

(defmacro with-symmetric (&body body)
  `(progn
     ,@body
     ,@(%make-symmetric body)))

(with-symmetric
  (defmacro %rb-insert-helper-left (Tree z)
    ;; Note that setf z means that this must be a macro!
    `(let ((y (node-right (node-parent (node-parent z)))))
       (cond
         ((red? y)
          (setf (node-color (node-parent z))  +black+)
          (setf (node-color y) +black+)
          (setf (node-color (node-parent (node-parent z)))
                +red+)
          (setf z
                (node-parent (node-parent z))))
         (t
          ;; Note that the CLRS algorithm is not printed very well here.
          (when (eq z (node-right (node-parent z)))
            (setf z (node-parent z))
            (left-rotate Tree z))
          (setf (node-color (node-parent z)) +black+)
          (setf (node-color (node-parent (node-parent z))) +red+)
          (right-rotate Tree (node-parent (node-parent z))))))))

(defun rb-insert-fixup (Tree z)
  (loop while (red? (node-parent z))
        do
           (cond
             ((eq (node-parent z)
                  (node-left (node-parent (node-parent z))))
              (%rb-insert-helper-left Tree z))
             (t
              (%rb-insert-helper-right Tree z))))
  (setf (node-color
         (rb-tree-root Tree))
        +black+))



(with-symmetric ;; exactly symeteric for right-rotate
 (defun left-rotate (Tree x)
   ;; See P. 313 CLRS
  
   (let ((y (node-right x))
         (T.nil (rb-tree-sentinel Tree))) ;; set y

     ;; turn y's left subtree into x's right subtree
     (setf (node-right x) (node-left y)) 
     (if (not (eq (node-left y) T.nil))
         (setf (node-parent
                (node-left y))
               x))

     ;; Link x's parent to y
     (setf (node-parent y) (node-parent x))

     (cond
       ((eq (node-parent x) T.nil)
        (setf (rb-tree-root Tree) y))
       ((eq x (node-left (node-parent x)))
        (setf (node-left (node-parent x)) y))
       (t
        (setf (node-right (node-parent x))
              y)))

     ;; Put x on y's left
     (setf (node-left y) x)
     (setf (node-parent x) y))))



(defun validate! (tree)
  (let ((sentinel (rb-tree-sentinel tree))
        (root (rb-tree-root tree)))
    ;; Property 1: Every node is either red or black (implicit in representation)
    
    ;; Property 2: The root is black
    (assert (black? root) () "Root must be black")
    
    ;; Property 3: All leaves (NIL/sentinel) are black
    (assert (black? sentinel) () "Sentinel must be black")
    
    ;; Helper function to validate properties 4 and 5
    (labels ((validate-node (node)
               (cond
                 ((eq node sentinel) 1) ; Black height of sentinel is 1
                 (t
                  ;; Property 4: If a node is red, both its children are black
                  (when (red? node)
                    (assert (black? (node-left node)) () 
                            "Red node must have black left child")
                    (assert (black? (node-right node)) () 
                            "Red node must have black right child"))
                  
                  ;; Recursively validate children and check black heights
                  (let ((left-height (validate-node (node-left node)))
                        (right-height (validate-node (node-right node))))
                    ;; Property 5: All paths have same black height
                    (assert (= left-height right-height) () 
                            "Black height mismatch at node ~A" (node-key node))
                    
                    ;; Return black height of current subtree
                    (if (black? node)
                        (1+ left-height)
                        left-height))))))
      
      (validate-node root))
    
    t))
