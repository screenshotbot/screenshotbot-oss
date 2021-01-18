(in-package :bknr.skip-list)

;;; TODO:
;;; index-protokoll fuer cursor
;;; range-queries

(defparameter *sl-random-state*
  (make-random-state t)
  "Internal status of the random number generator.")

(defun sl-random ()
  "Pseudo-random number generator, returns NIL 3/4 of the time."
  (declare (optimize (speed 3)))
  (< 2 (random 4 *sl-random-state*)))

(defconstant +max-level+ (the fixnum 32)
  "Maximum level of skip-list, should be enough for 2^32 elements.")

(defun random-level ()
  "Returns a random level for a new skip-list node, with SL-RANDOM p for each level."
  (declare (optimize speed))
  (do ((level 1 (1+ level)))
      ((or (= level +max-level+)
	   (sl-random)) level)
    (declare (type fixnum level))))

;;; A node is a SIMPLE-VECTOR containing KEY, VALUE and the forward pointers

(defmacro node-key (node)
  `(aref (the simple-vector ,node) 0))

(defmacro node-value (node)
  `(aref ,node 1))

(defmacro node-forward (node &optional (i 0))
  `(aref (the simple-vector ,node) (the fixnum (+ ,i 2))))

(defun make-node (key value size &key initial-element)
  (let ((node (make-array (+ 2 size) :initial-element initial-element)))
    (setf (node-key node) key
	  (node-value node) value)
    node))

(defun node-level (node)
  (declare (type simple-vector node))
  (- (length node) 2))

(defun make-header (&key initial-element)
  (make-node nil nil +max-level+ :initial-element initial-element))

(defclass skip-list ()
  ((header :initform (make-header)
	   :reader skip-list-header :type simple-vector)
   #+nil
   (finger :initform (make-header)
	   :reader skip-list-finger :type simple-vector)
   (length :initform 0 :accessor skip-list-length :type fixnum)
   (level :initform 0 :accessor skip-list-level :type fixnum))
  (:documentation "Skip-list class, access to elements is done through the header node."))

(defmethod print-object ((sl skip-list) stream)
  (print-unreadable-object (sl stream :type t :identity t)
    (format stream "length = ~A" (skip-list-length sl))))

(defmethod skip-list-empty-p ((sl skip-list))
  (= (skip-list-length sl) 0))

(defun follow-node (node key level)
  "Follow a skip-list node at level LEVEL, stopping when there is no
next node or when the next node has a larger key."
  (declare (type (or null simple-vector) node)
	   (type integer key)
	   (type integer level)
	   (optimize (speed 3)))
  (do ((next (node-forward node level) (node-forward node level)))
      ((not (and next (< (node-key next) key))) node)
    (declare (type (or null simple-vector) next))
    (setf node next)))

(defmethod make-update ((sl skip-list) key)
  "Search the list for the node before KEY, and construct an update
array storing the previous nodes at all the levels."
  (declare (type integer key)
	   (optimize (speed 3)))
  (let ((node (skip-list-header sl))
	(update (make-header :initial-element (skip-list-header sl))))
    (declare (type (or null simple-vector) node)
	     (type simple-vector update)
	     (optimize (speed 3)))
    
    (do ((level (1- (skip-list-level sl)) (1- level)))
	((< level 0) (values update (node-forward node 0)))
      (declare (type integer level))

      (setf (node-forward update level)
	    (setf node (follow-node node key level))))))

    
(defmethod skip-list-insert ((sl skip-list) key value)
  "Insert VALUE under KEY in the skip-list. Replaces the existing node
with KEY if present, or inserts a new node with random level."

  (multiple-value-bind (update node)
      (make-update sl key)
    #+nil
    (format t "update after first search: ~A~%" update)
    
    (if (and node (= (node-key node) key))
	;; ein knoten mit dem KEY existiert schon, nur VALUE veraendern
	(setf (node-value node) value)

	;; es muss ein neuer knoten mit zufaelligem LEVEL eingefuegt werden
	(let* ((new-level (random-level))
	       (new-node (make-node key value new-level)))

	  #+nil
	  (format t "new-node ~A~%" new-node)

	  (when (> new-level (skip-list-level sl))
	    (setf (skip-list-level sl) new-level))
	  (incf (skip-list-length sl))

	  ;; einfuegen des neuen knoten, indem der knoten in die
	  ;; jeweiligen previous knoten an naechster stelle
	  ;; eingetragen wird (dazu war UPDATE gut)
	  (do ((level 0 (1+ level)))
	      ((= level new-level) new-node)
	    
	    (let ((next (node-forward update level)))
	      (when next
		(setf (node-forward new-node level) (node-forward next level)
		      (node-forward next level)      new-node))))))

    value))

(defmethod skip-list-reduce-header ((sl skip-list))
  "Sets the LEVEL slot of the skip-list according to the length of the header node."
  ;; level slot der skip-liste anpassen an die header struktur
  (do ((level (1- (skip-list-level sl)) (1- level))
       (header (skip-list-header sl)))
      ((or (< level 0)
	   (node-forward header level))
       (setf (skip-list-level sl) (1+ level))
       sl)))

(defmethod skip-list-delete ((sl skip-list) key)
  "Delete the node with KEY in the skip-list."
  (multiple-value-bind (update node)
      (make-update sl key)

    (when (and node (= (node-key node) key))
      (do ((level 0 (1+ level)))
	  ((= level (skip-list-level sl)))
	(let ((next (node-forward update level)))
	  (when (and next (eql (node-forward next level) node))
	    (setf (node-forward next level) (node-forward node level)))))

      (decf (skip-list-length sl))
      (skip-list-reduce-header sl))
    
    sl))

;;; compatibility to old API

(defmethod skip-list-get (key (sl skip-list))
  (skip-list-search sl key))

(defmethod (setf skip-list-get) (new-value key (sl skip-list))
  (skip-list-insert sl key new-value))

(defmethod skip-list-remove (key (sl skip-list))
  (skip-list-delete sl key))

(defmethod skip-list-search-node ((sl skip-list) key)
  "Search for the node with KEY in the skip-list."
  (declare (type integer key)
	   (optimize (speed 3)))
  (do ((level (1- (skip-list-level sl)) (1- level))
       (node (skip-list-header sl) (follow-node node key level)))
      ((< level 0)
       (let ((result (node-forward node)))
	 (if (and result (= (node-key result) key))
	     result
	     nil)))
    (declare (type fixnum level)
	     (type simple-vector node))))

(defmethod skip-list-after-node ((sl skip-list) key)
  "Search for the node with key biffer or equal to KEY in the skip-list (for range queries)."
  (declare (type integer key)
	   (optimize (speed 3)))
  (do ((level (1- (skip-list-level sl)) (1- level))
       (node (skip-list-header sl) (follow-node node key level)))
      ((< level 0)
       (let ((result (node-forward node)))
	 (if (and result (>= (node-key result) key))
	     result
	     nil)))
    (declare (type fixnum level)
	     (type simple-vector node))))

(defmethod skip-list-search ((sl skip-list) key &optional not-found)
  (let ((result (skip-list-search-node sl key)))
    (if result
	(node-value result)
	not-found)))

(defun node-before (node key &optional (level 0))
  (let ((next (node-forward node level)))
    (and next (< (node-key next) key))))

;; broken, and not necessarily faster
#+nil
(defmethod skip-list-search ((sl skip-list) key &optional not-found)
  (let ((finger (skip-list-finger sl))
	node
	lvl)

    (if (node-before finger key 0)

	(do ((level 1 (1+ level)))
	    ((or (>= level (skip-list-level sl))
		 (not (node-before finger key level)))
	     (setf node (node-forward finger (1- level))
		   lvl (1- level))))

	(do ((level 1 (1+ level)))
	    ((or (>= level (skip-list-level sl))
		 (node-before finger key level))
	     
	     (if (>= level (skip-list-level sl))
		 (setf node (skip-list-header sl)
		       lvl (1- (skip-list-level sl)))
		 (setf node (node-forward finger level)
		       lvl level)))
	  #+nil(format t "level: ~A~%" level)))

    #+nil(format t "node: ~A, level ~A~%" node lvl)

    (do ((level lvl (1- level)))
	((or (null node)
	     (< level 0))
	 (when node (setf node (node-forward node 0))))

      (setf node (follow-node node key level))
      (unless (eql node (skip-list-header sl))
	#+nil(format t "storing in finger~%")
	(setf (node-forward finger level) node)))

    (if (and node (= (node-key node) key))
	(node-value node)
	not-found)))

(defmethod skip-list-to-list ((sl skip-list))
  (let ((node (skip-list-header sl)))
    (loop for next = (node-forward node) then (node-forward next)
	  while next
	  collect (list (node-key next) (node-value next)))))

;;; cursors

(defclass skip-list-cursor ()
  ((node :initarg :node :accessor skip-list-cursor-node)))

(defmethod sl-cursor-next ((slc skip-list-cursor) &optional eoc)
  (with-slots (node) slc
    (if node
	(let ((result (list (bknr.skip-list::node-key node)
			    (bknr.skip-list::node-value node))))
	  (setf node (bknr.skip-list::node-forward node))
	  result)
	eoc)))

(defmethod sl-cursor-prev ((slc skip-list-cursor) &optional eoc)
  (declare (ignore eoc))
  (error "Can not move backward in skip-list"))

(defclass skip-list-value-cursor (skip-list-cursor)
  ())

(defmethod sl-cursor-next :around ((slc skip-list-value-cursor) &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
	eoc
	(second result))))

(defclass skip-list-key-cursor (skip-list-cursor)
  ())

(defmethod sl-cursor-next :around ((slc skip-list-key-cursor) &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
	eoc
	(first result))))

(defmethod skip-list-cursor ((sl skip-list) &key cursor (class 'skip-list-cursor))
  (if cursor
      (progn (setf (skip-list-cursor-node cursor)
		   (bknr.skip-list::node-forward (bknr.skip-list::skip-list-header sl)))
	     cursor)
      (make-instance class :node (bknr.skip-list::node-forward (bknr.skip-list::skip-list-header sl)))))

(defmethod skip-list-values-cursor ((sl skip-list))
  (skip-list-cursor sl :class 'skip-list-value-cursor))

(defmethod skip-list-keys-cursor ((sl skip-list))
  (skip-list-cursor sl :class 'skip-list-key-cursor))

(defclass skip-list-range-cursor (skip-list-cursor)
  ((end :initarg :end :reader slrc-end)))

(defmethod sl-cursor-next :around ((slc skip-list-range-cursor) &optional eoc)
  (with-slots (node end) slc
    (if (and node (< (bknr.skip-list::node-key node) end))
	(call-next-method)
	eoc)))

(defmethod skip-list-range-cursor ((sl skip-list) start end)
  (let ((node (bknr.skip-list::skip-list-after-node sl start)))
    (when node
      (make-instance 'skip-list-range-cursor :node node :end end))))

(defmethod map-skip-list (fun (sl skip-list))
  (let ((cursor (skip-list-cursor sl)))
    (do ((val (sl-cursor-next cursor) (sl-cursor-next cursor)))
	((null val))
      (apply fun val))))

(defmethod map-skip-list-values (fun (sl skip-list))
  (let ((cursor (skip-list-values-cursor sl)))
    (do ((val (sl-cursor-next cursor) (sl-cursor-next cursor)))
	((null val))
      (funcall fun val))))

