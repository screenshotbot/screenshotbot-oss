;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-rb-tree
  (:use #:cl
        #:fiveam)
  (:import-from #:util/rb-tree
                #:rb-delete
                #:node-key
                #:sorted-keys
                #:validate!
                #:+black+
                #:+red+
                #:%rb-insert-only
                #:rb-insert
                #:rb-tree-root
                #:rb-tree-sentinel
                #:node-parent
                #:node-left
                #:node-right
                #:make-node
                #:make-tree))
(in-package :util/tests/test-rb-tree)


(util/fiveam:def-suite)

(defun fix-color (color)
  (ecase color
    (red
     +red+)
    (black
     +black+)))

(defun %make-test-node (tree node-expr)
  "Node expr looks like: (color value <left node expr> <right node expr>)"
  (if (null node-expr)
      (rb-tree-sentinel tree)
      (let ((color (first node-expr))
            (value (second node-expr))
            (left-expr (third node-expr))
            (right-expr (fourth node-expr)))
        (let ((node (make-node :color (fix-color color)
                               :key value
                               :parent nil)))
          (setf (node-left node) (%make-test-node tree left-expr))
          (setf (node-right node) (%make-test-node tree right-expr))
          (unless (eq (node-left node) (rb-tree-sentinel tree))
            (setf (node-parent (node-left node)) node))
          (unless (eq (node-right node) (rb-tree-sentinel tree))
            (setf (node-parent (node-right node)) node))
          node))))

(defun tree= (tree1 tree2)
  (labels ((nodes-equal (node1 node2)
             (cond
               ((or (not node1)
                    (not node2))
                (error "Found a NIL node (not sentinel!) in the tree"))
               ((and (eq node1 (rb-tree-sentinel tree1))
                     (eq node2 (rb-tree-sentinel tree2)))
                t)
               ((or (eq node1 (rb-tree-sentinel tree1))
                    (eq node2 (rb-tree-sentinel tree2)))
                nil)
               (t
                (and (eq (slot-value node1 'util/rb-tree::color)
                         (slot-value node2 'util/rb-tree::color))
                     (equal (slot-value node1 'util/rb-tree::key)
                            (slot-value node2 'util/rb-tree::key))
                     (nodes-equal (node-left node1) (node-left node2))
                     (nodes-equal (node-right node1) (node-right node2)))))))
    (nodes-equal (rb-tree-root tree1) (rb-tree-root tree2))))

(defmacro tree (body)
  `(let ((tree (make-tree)))
     (setf (rb-tree-root tree)
           (%make-test-node tree ',body))
     tree))

(test construction-of-test-trees
  (finishes
   (tree
    (black 2)))
  (finishes
   (tree
    (black 2
           (red 1)
           (red 4)))))

(defun make-big-tree ()
  )

(test tree=
  (is-true
   (tree=
       (tree
        (black 2
               (red 1)
               (red 4)))
       (tree
        (black 2
               (red 1)
               (red 4))))))

(test simple-construction
  (finishes
    (make-tree)))

(test insert-when-nothing-is-present-without-fixup
  (let ((tree (make-tree)))
    (%rb-insert-only tree (make-node :key 2))
    (is-true
     (tree=
      (tree
       (red 2))
      tree))))

(test insert-when-nothing-is-present
  (let ((tree (make-tree)))
    (rb-insert tree (make-node :key 2))
    (is-true
     (tree=
      (tree
       (black 2))
      tree))))

(test insert-in-deterministic-order
  (let ((tree (make-tree)))
    (rb-insert tree (make-node :key 1))
    (rb-insert tree (make-node :key 2))
    (rb-insert tree (make-node :key 3))
    (rb-insert tree (make-node :key 4))
    (rb-insert tree (make-node :key 5))
    (rb-insert tree (make-node :key 6))    
    (rb-insert tree (make-node :key -1))
    (finishes (validate! tree))))

(test insert-a-bunch-of-random-elements
  (let ((tree (make-tree)))
    (rb-insert tree (make-node :key 42))
    (rb-insert tree (make-node :key 17))
    (rb-insert tree (make-node :key 89))
    (rb-insert tree (make-node :key 3))
    (rb-insert tree (make-node :key 56))
    (finishes (validate! tree))
    (rb-insert tree (make-node :key 91))
    (rb-insert tree (make-node :key 24))
    (rb-insert tree (make-node :key 7))
    (rb-insert tree (make-node :key 68))
    (rb-insert tree (make-node :key 35))
    (finishes (validate! tree))
    (rb-insert tree (make-node :key 12))
    (rb-insert tree (make-node :key 73))
    (rb-insert tree (make-node :key 46))
    (rb-insert tree (make-node :key 81))
    (rb-insert tree (make-node :key 29))
    (rb-insert tree (make-node :key 64))
    (rb-insert tree (make-node :key 15))
    (rb-insert tree (make-node :key 97))
    (rb-insert tree (make-node :key 52))
    (rb-insert tree (make-node :key 38))
    (finishes (validate! tree))))

(test ordering
  (let ((tree (make-tree)))
    (rb-insert tree (make-node :key 42))
    (rb-insert tree (make-node :key 17))
    (rb-insert tree (make-node :key 89))
    (rb-insert tree (make-node :key 3))
    (rb-insert tree (make-node :key 56))
    (is (equal
         '(3 17 42 56 89)
         (sorted-keys tree)))))

(test respects-the-comparison-operator
  (let ((tree (make-tree :cmp #'>)))
    (rb-insert tree (make-node :key 42))
    (rb-insert tree (make-node :key 17))
    (rb-insert tree (make-node :key 89))
    (rb-insert tree (make-node :key 3))
    (rb-insert tree (make-node :key 56))
    (is (equal
         (reverse '(3 17 42 56 89))
         (sorted-keys tree)))))


(test insert-100-random-elements
  (let ((tree (make-tree))
        (elements (make-array 100)))
    ;; Generate 100 random unique elements
    (loop for i from 0 below 100
          do (setf (aref elements i) (random 10000)))
    ;; Remove duplicates by converting to list, removing duplicates, and back
    (let ((unique-elements (remove-duplicates (coerce elements 'list))))
      ;; Insert all elements
      (dolist (element unique-elements)
        (rb-insert tree (make-node :key element)))
      ;; Validate the tree structure using existing validate! function
      (finishes (validate! tree)))))

(test insert-and-delete-20-numbers-random-order
  (let ((tree (make-tree))
        (numbers (loop for i from 1 to 20 collect i)))
    ;; Insert 20 numbers in order
    (dolist (num numbers)
      (rb-insert tree (make-node :key num)))
    
    ;; Validate tree after all insertions
    (finishes (validate! tree))
    (is (equal numbers (sorted-keys tree)))
    
    ;; Create a shuffled copy for deletion order
    (let ((delete-order (copy-list numbers)))
      ;; Shuffle the list using Fisher-Yates algorithm
      (loop for i from (1- (length delete-order)) downto 1 do
        (let ((j (random (1+ i))))
          (rotatef (nth i delete-order) (nth j delete-order))))
      
      ;; Delete numbers in random order
      (let ((remaining (copy-list numbers)))
        (dolist (num delete-order)
          ;; Find and delete the node with this key
          (labels ((find-node (node)
                     (cond
                       ((eq node (rb-tree-sentinel tree)) nil)
                       ((= (node-key node) num) node)
                       ((< num (node-key node)) (find-node (node-left node)))
                       (t (find-node (node-right node))))))
            (let ((node-to-delete (find-node (rb-tree-root tree))))
              (is-true node-to-delete)
              (when node-to-delete
                (rb-delete tree node-to-delete)
                (setf remaining (remove num remaining))
                ;; Validate tree structure after each deletion
                (finishes (validate! tree))
                ;; Check that remaining keys are still in sorted order
                (is (equal remaining (sorted-keys tree)))))))))))

(test tree-minimum
  (let ((tree (make-tree)))
    ;; Test empty tree
    ;;(is (eq nil (util/rb-tree::tree-minimum tree (rb-tree-root tree))))
    
    ;; Test single node tree
    (rb-insert tree (make-node :key 42))
    (is (= 42 (node-key (util/rb-tree::tree-minimum tree (rb-tree-root tree)))))
    
    ;; Test tree with multiple nodes
    (rb-insert tree (make-node :key 17))
    (rb-insert tree (make-node :key 89))
    (rb-insert tree (make-node :key 3))
    (rb-insert tree (make-node :key 56))
    (is (= 3 (node-key (util/rb-tree::tree-minimum tree (rb-tree-root tree)))))
    
    ;; Test with negative numbers
    (rb-insert tree (make-node :key -10))
    (rb-insert tree (make-node :key -5))
    (is (= -10 (node-key (util/rb-tree::tree-minimum tree (rb-tree-root tree)))))
    
    ;; Test with custom comparator (reverse order)
    (let ((reverse-tree (make-tree :cmp #'>)))
      (rb-insert reverse-tree (make-node :key 42))
      (rb-insert reverse-tree (make-node :key 17))
      (rb-insert reverse-tree (make-node :key 89))
      (rb-insert reverse-tree (make-node :key 3))
      (is (= 89 (node-key (util/rb-tree::tree-minimum reverse-tree (rb-tree-root reverse-tree))))))))

