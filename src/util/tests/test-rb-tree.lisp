;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-rb-tree
  (:use #:cl
        #:fiveam)
  (:import-from #:util/rb-tree
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
