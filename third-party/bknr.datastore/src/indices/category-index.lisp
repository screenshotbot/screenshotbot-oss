(in-package :bknr.indices)

;;; category tree structure

(defun make-node (name children)
  (cons name children))

(defun node-name (node)
  (car node))

(defun (setf node-name) (new-value node)
  (setf (car node) new-value))

(defun node-children (node)
  (cdr node))

(defun node-children-empty-p (node)
  (null (node-children node)))

(defun (setf node-children) (new-value node)
  (setf (cdr node) new-value))

(defstruct category-tree
  (test #'eql)
  (root-node (make-node :root nil)))

(defun node-to-categories (node &optional parent-category)
  (let ((category (append parent-category (list (node-name node)))))
    (cons category (mapcan #'(lambda (child) (node-to-categories child category))
			   (node-children node)))))

(defun nodes-to-categories (nodes &optional parent-category)
  (mapcan #'(lambda (node) (node-to-categories node parent-category)) nodes))

(defun tree-categories (tree &optional category)
  (nodes-to-categories (node-children (category-tree-root-node tree)) category))

(defun tree-find-node (tree category)
  (unless (listp category)
    (setf category (list category)))
  (do* ((curnode (category-tree-root-node tree)
		 (find catname (node-children curnode)
		       :key #'node-name
		       :test (category-tree-test tree)))
	(curcat category (cdr curcat))
	(catname (car curcat) (car curcat)))
       ((or (null curnode)
	    (null curcat))
	curnode)))

(defun category-to-node (category)
  (if (null category)
      nil
      (let ((child (category-to-node (cdr category))))
	    (make-node (first category)
		       (when child (list child))))))

(defun tree-add-category (tree category)
  (unless (listp category)
    (setf category (list category)))
  (do* ((curnode (category-tree-root-node tree))
	(curcat category (cdr curcat))
	(catname (car curcat) (car curcat)))
       ((or (null curnode)
	    (null curcat))
	tree)
    (let ((node (find catname (node-children curnode)
		      :key #'node-name
		      :test (category-tree-test tree))))
      (if node
	  (setf curnode node)
	  (progn
	    (push (category-to-node curcat)
		  (node-children curnode))
	    (return-from tree-add-category tree))))))

(defun tree-remove-category (tree category)
  (unless (listp category)
    (setf category (list category)))
  (when category
    (let* ((parent-category (parent-category category))
	   (parent-node (tree-find-node tree parent-category)))
      (when parent-node
	(setf (node-children parent-node)
	      (remove (category-name category)
		      (node-children parent-node)
		      :key #'car
		      :test (category-tree-test tree)))
	(when (node-children-empty-p parent-node)
	  (tree-remove-category tree parent-category)))))
  tree)

(defun parent-categories (category)
  (let (res)
    (dotimes (i (1-  (length category)))
      (push (butlast category (1+ i)) res))
    res))

(defun parent-category (category)
  (butlast category 1))

(defun category-name (category)
  (car (last category)))

(defun tree-find-children (tree category)
  (nodes-to-categories (node-children (tree-find-node tree category)) category))

(defun tree-find-siblings (tree category)
  (let ((len (length category)))
    (if (<= len 1)
	tree
	(let ((sib-cat (subseq category 0 (1- (length category)))))
	  (nodes-to-categories (tree-find-children tree sib-cat) sib-cat)))))

;;; category index

(defclass category-index (hash-index)
  ((tree :initform (make-category-tree)
	 :initarg :tree
	 :accessor category-index-tree))
  (:default-initargs :test #'equal))

(defmethod initialize-instance :after ((index category-index) &key (tree-test #'eql))
  (with-slots (tree) index
    (setf tree (make-category-tree :test tree-test))))

(defmethod index-get ((index category-index) category)
  (let* ((tree (category-index-tree index))
	 (hash (slot-index-hash-table index))
	 (categories (cons category
			   (tree-find-children tree category))))
    (mapcan #'(lambda (category)
		(copy-list (gethash category hash))) categories)))

(defmethod index-add ((index category-index) object)
  (unless (slot-boundp object (slot-index-slot-name index))
    (return-from index-add))
  (let ((key (slot-value object (slot-index-slot-name index)))
	(hash-table (slot-index-hash-table index))
	(tree (category-index-tree index)))
    (when (and (not (slot-index-index-nil index))
	       (null key))
      (return-from index-add))
    (if (nth-value 1 (gethash key hash-table))
        (push object (gethash key hash-table))
        (progn
          (tree-add-category tree key)
          (setf (gethash key hash-table) (list object))))))

(defmethod index-remove ((index category-index) object)
  (let ((key (slot-value object (slot-index-slot-name index)))
	(hash-table (slot-index-hash-table index))
	(tree (category-index-tree index)))
    (let ((new-value (delete-first object (gethash key hash-table))))
      (if (null new-value)
	  (progn
	    (tree-remove-category tree key)
	    (remhash key hash-table))
	  (setf (gethash key hash-table) new-value)))))

(defmethod index-keys ((index category-index))
  (tree-categories (category-index-tree index)))

(defmethod index-reinitialize :around ((new-index category-index)
				       (old-index category-index))
  (let* ((new-index (call-next-method))
	 (tree (category-index-tree new-index))
	 (new-hash (slot-index-hash-table new-index)))
    (loop for key being the hash-key of new-hash
	  do (tree-add-category tree key))
    new-index))

#|

(defclass image ()
  ((category :index-type category-index
	     :index-reader images-with-category
	     :index-keys all-image-categories
	     :index-var *image-category-index*
	     :initarg :category
	     :reader image-category))
  (:metaclass indexed-class))

(make-instance 'image :category '(:photo :stills :nature))
(make-instance 'image :category '(:photo :naked :woman))
(make-instance 'image :category '(:painting :abstract :cubist))

(defclass track ()
  ((category :index-type category-index
	     :index-initargs (:tree-test #'equal)
	     :index-reader tracks-with-category
	     :index-keys all-track-categories
	     :index-var *track-category-index*
	     :initarg :category
	     :reader track-category))
  (:metaclass indexed-class))

(make-instance 'track :category '("Rock" "New-Age" "Noise"))
(make-instance 'track :category '("Rock" "New-Age" "Techno"))
	     

|#
