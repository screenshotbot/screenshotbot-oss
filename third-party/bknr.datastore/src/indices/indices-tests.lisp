(eval-when (:compile-toplevel :load-toplevel :execute)
  (or (find-package :bknr.indices.tests)
      (defpackage :bknr.indices.tests
	(:use :cl :bknr.indices :unit-test))))

(in-package :bknr.indices.tests)

(define-test-class index-test-class
    ((index :initarg :index)))

(defvar *test-index* nil)

(defmethod run-test :around ((test index-test-class) &optional (output *debug-io*))
  (declare (ignore output))
  (let ((*test-index* (slot-value test 'index)))
    (index-clear *test-index*)
    (call-next-method)))

(defmacro define-index-test (name (&rest index-initargs) &rest body)
  `(make-instance 'index-test-class
    :unit :index
    :name ',name
    :index (index-create ,@index-initargs)
    :body #'(lambda () ,@body)))

(defclass indexed-object ()
  ((a :initarg :a :reader indexed-object-a)
   (b :initarg :b :reader indexed-object-b)
   (c :initarg :c :reader indexed-object-c)
   (d :initarg :d :reader indexed-object-d)))

(defun test-index-add ()
  (let ((a-obj (make-instance 'indexed-object :a 3 :b 3 :c 3 :d 3)))
    (index-add *test-index* a-obj)
    (test-equal (index-get *test-index* 3) a-obj)
    (index-add *test-index* a-obj)
    (test-equal (index-get *test-index* 3) a-obj)
    (test-equal (index-values *test-index*) (list a-obj))))

(defun test-index-existing ()
  (let ((a (make-instance 'indexed-object :a 3 :b 3 :c 3 :d 3))
	(b (make-instance 'indexed-object :a 3 :b 3 :c 3 :d 3)))
    (index-add *test-index* a)
    (test-condition (index-add *test-index* b)
		    'index-existing-error)))

(defun test-index-remove ()
  (let ((a (make-instance 'indexed-object :a 3 :b 3 :c 3 :d 3)))
    (index-add *test-index* a)
    (test-equal (index-get *test-index* 3) a)
    (index-remove *test-index* a)
    (test-equal (index-get *test-index* 3) nil)
    (index-add *test-index* a)
    (test-equal (index-get *test-index* 3) a)
    (index-remove *test-index* a)
    (test-equal (index-get *test-index* 3) nil)
    (let ((b (make-instance 'indexed-object :a 4 :b 4 :c 4 :d 4)))
      (index-add *test-index* b)
      (test-equal (index-get *test-index* 4) b)
      (index-remove *test-index* b)
      (test-equal (index-get *test-index* 4) nil))))

(define-index-test unique-index-create ('unique-index :slots '(a))
  (test-assert *test-index*))

(define-index-test unique-index-add ('unique-index :slots '(a))
  (test-index-add))

(define-index-test unique-index-index-existing ('unique-index :slots '(a))
  (test-index-existing))

(define-index-test unique-index-remove ('unique-index :slots '(a))
  (test-index-remove))

(define-index-test unique-index-add2 ('unique-index :slots '(b))
  (test-index-add))

(define-index-test unique-index-index-existing2 ('unique-index :slots '(b))
  (test-index-existing))

(define-index-test unique-index-remove2 ('unique-index :slots '(b))
  (test-index-remove))

(define-index-test unique-index-reinitialize ('unique-index :slots '(a))
  (let ((a-obj (make-instance 'indexed-object :a 3 :b 3 :c 3 :d 3)))
    (index-add *test-index* a-obj)
    (test-equal (index-get *test-index* 3) a-obj)
    (let ((new-index (index-create 'unique-index :slots '(a))))
      (index-reinitialize new-index *test-index*)
      (test-equal (index-get *test-index* 3) a-obj)
      (test-equal (index-values *test-index*)
		  (index-values new-index)))
    (let ((new-index (index-create 'unique-index :slots '(a) :test #'eq)))
      (index-reinitialize new-index *test-index*)
      (test-equal (index-get *test-index* 3) a-obj)
      (test-equal (index-values *test-index*)
		  (index-values new-index)))))

(define-index-test unique-index-stress ('unique-index :slots '(a))
  (dotimes (i 10000)
    (index-add *test-index* (make-instance 'indexed-object :a i :b i :c i :d i)))
  (test-equal (length (index-values *test-index*)) 10000)
  (index-mapvalues *test-index* #'(lambda (obj) (index-remove *test-index* obj)))
  (test-equal (index-values *test-index*) nil))

(define-test-class indexed-class-test-class
    ((classes :initarg :classes)))

(defmethod run-test :before ((test indexed-class-test-class) &optional (output *debug-io*))
  (declare (ignore output))
  (let ((classes (slot-value test 'classes)))
    (map nil #'clear-class-indices (mapcar #'find-class classes))))

(defmacro define-indexed-class-test (name (&rest classes) &rest body)
  `(make-instance 'indexed-class-test-class
    :unit :index
    :name ',name
    :classes ',classes
    :body #'(lambda () ,@body)))

(defclass gorilla ()
  ((name :initarg :name :reader gorilla-name
	 :index-type unique-index :index-initargs (:test #'equal)
	 :index-reader gorilla-with-name :index-values all-gorillas)
   (description :initarg :description :accessor gorilla-description
		:index-type hash-index
		:index-reader gorillas-with-description))
  (:metaclass indexed-class))

(define-indexed-class-test gorilla-create (gorilla)
  (let ((john (make-instance 'gorilla :name "John" :description :aggressive))
	(lucy (make-instance 'gorilla :name "Lucy" :description :aggressive))
	(robert (make-instance 'gorilla :name "Robert" :description :playful)))
    (test-equal (length (all-gorillas)) 3)
    (test-assert (member john (all-gorillas)))
    (test-assert (member lucy (all-gorillas)))
    (test-assert (member robert (all-gorillas)))

    (test-equal (length (gorillas-with-description :aggressive)) 2)
    (test-assert (member lucy (gorillas-with-description :aggressive)))
    (test-assert (member john (gorillas-with-description :aggressive)))

    (test-equal (gorillas-with-description :playful) (list robert))))

(define-indexed-class-test gorilla-setf (gorilla)
  (let ((john (make-instance 'gorilla :name "John" :description :aggressive))
	(lucy (make-instance 'gorilla :name "Lucy" :description :aggressive))
	(robert (make-instance 'gorilla :name "Robert" :description :playful)))
    (test-equal (length (all-gorillas)) 3)
    (test-assert (member john (all-gorillas)))
    (test-assert (member lucy (all-gorillas)))
    (test-assert (member robert (all-gorillas)))

    (test-equal (length (gorillas-with-description :aggressive)) 2)
    (test-assert (member lucy (gorillas-with-description :aggressive)))
    (test-assert (member john (gorillas-with-description :aggressive)))

    (test-equal (gorillas-with-description :playful) (list robert))

    (setf (gorilla-description lucy) :playful)

    (test-equal (length (gorillas-with-description :playful)) 2)
    (test-assert (member lucy (gorillas-with-description :playful)))
    (test-assert (member robert (gorillas-with-description :playful)))

    (test-equal (gorillas-with-description :aggressive) (list john))

    (test-condition (setf (slot-value lucy 'name) "Robert") 'index-existing-error)))

(define-indexed-class-test gorilla-destroy (gorilla)
  (let ((john (make-instance 'gorilla :name "John" :description :aggressive))
	(lucy (make-instance 'gorilla :name "Lucy" :description :aggressive))
	(robert (make-instance 'gorilla :name "Robert" :description :playful)))
    (test-equal (length (all-gorillas)) 3)
    (test-assert (member john (all-gorillas)))
    (test-assert (member lucy (all-gorillas)))
    (test-assert (member robert (all-gorillas)))

    (test-equal (length (gorillas-with-description :aggressive)) 2)
    (test-assert (member lucy (gorillas-with-description :aggressive)))
    (test-assert (member john (gorillas-with-description :aggressive)))

    (test-equal (gorillas-with-description :playful) (list robert))

    (destroy-object lucy)
    (test-equal (gorillas-with-description :playful) (list robert))
    (test-equal (gorillas-with-description :aggressive) (list john))
    (test-equal (length (all-gorillas)) 2)
    (test-assert (member john (all-gorillas)))
    (test-assert (member robert (all-gorillas)))

    (test-condition (gorilla-name lucy) 'error)))

(defclass gorilla2 ()
  ((name :initarg :name :reader gorilla2-name)
   (description :initarg :description :reader gorilla2-description)
   (x :initarg :x :reader gorilla2-x)
   (y :initarg :y :reader gorilla2-y))
  (:metaclass indexed-class)
  (:class-indices (coords :index-type array-index
			  :slots (x y)
			  :index-reader gorilla2-with-coords
			  :index-initargs (:dimensions '(256 256)))))

(define-indexed-class-test gorilla2-create (gorilla2)
  (let ((john (make-instance 'gorilla2 :name "John" :description :aggressive :x 5 :y 8))
	(lucy (make-instance 'gorilla2 :name "Lucy" :description :aggressive :x 6 :y 9))
	(robert (make-instance 'gorilla2 :name "Robert" :description :playful :x 7 :y 10)))
    (test-equal john (gorilla2-with-coords '(5 8)))
    (test-equal lucy (gorilla2-with-coords '(6 9)))
    (test-equal robert (gorilla2-with-coords '(7 10)))
    (with-slots (x y) lucy
      (setf x 0 y 0))
    (test-equal lucy (gorilla2-with-coords '(0 0)))

    (test-condition (with-slots (x y) lucy
		      (setf x 7 y 10)) 'index-existing-error)

    (destroy-object john)
    (test-equal (gorilla2-with-coords '(5 8)) nil)))

(defclass test-slot ()
  ((a :initarg :a :index-type unique-index
      :reader test-slot-a
      :index-reader test-slot-with-a
      :index-values all-test-slots)
   (b :initarg :b :index-type unique-index
      :index-reader test-slot-with-b
      :index-subclasses nil
      :index-values all-test-slots-bs))
  (:metaclass indexed-class))

(defclass test-slot2 (test-slot)
  ((b :initarg :b :index-type unique-index
      :index-reader test-slot2-with-b
      :index-subclasses nil
      :index-mapvalues map-test-slot2s
      :index-values all-test-slot2s-bs))
  (:metaclass indexed-class))

(define-indexed-class-test test-slot-indices (test-slot test-slot2)
  (let ((t1 (make-instance 'test-slot :a 1 :b 2))
	(t2 (make-instance 'test-slot :a 2 :b 3))
	(t3 (make-instance 'test-slot2 :a 3 :b 4))
	(t4 (make-instance 'test-slot2 :a 4 :b 2))
	(t5 (make-instance 'test-slot2 :a 5 :b 9)))
    (test-equal (length (all-test-slots)) 5)
    (test-assert (subsetp (list t1 t2 t3 t4 t5) (all-test-slots)))
    (test-equal (length (all-test-slots-bs)) 2)
    (test-assert (subsetp (list t1 t2) (all-test-slots-bs)))
    (test-equal (test-slot-with-a 2) t2)
    (test-equal (length (all-test-slot2s-bs)) 3)
    (test-assert (subsetp (list t3 t4 t5) (all-test-slot2s-bs)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *existing-unique-index*
    (index-create 'unique-index :slots '(a))))

(defclass test-slot3 ()
  ((a :initarg :a :index *existing-unique-index*))
  (:metaclass indexed-class))

(define-indexed-class-test existing-unique-index (test-slot3)
  (let ((t1 (make-instance 'test-slot3 :a 3))
	(t2 (make-instance 'test-slot3 :a 4)))
    (test-equal (index-get *existing-unique-index* 4) t2)
    (test-equal (length (index-values *existing-unique-index*)) 2)
    (test-assert (subsetp (list t1 t2) (index-values *existing-unique-index*)))))

(defclass test-slot4 (test-slot)
  ((a :initarg :a :index-type unique-index :index-reader test-slot4-wit-a
      :index-values all-test-slot4s))
  (:metaclass indexed-class))

(define-indexed-class-test test-slot-indices2 (test-slot test-slot2 test-slot4)
  (let ((t1 (make-instance 'test-slot :a 1 :b 2))
	(t2 (make-instance 'test-slot :a 2 :b 3))
	(t3 (make-instance 'test-slot2 :a 3 :b 4))
	(t4 (make-instance 'test-slot2 :a 4 :b 2))
	(t5 (make-instance 'test-slot2 :a 5 :b 9)))
    (test-equal (length (all-test-slots)) 5)
    (test-assert (subsetp (list t1 t2 t3 t4 t5) (all-test-slots)))
    (test-equal (length (all-test-slots-bs)) 2)
    (test-assert (subsetp (list t1 t2) (all-test-slots-bs)))
    (test-equal (test-slot-with-a 2) t2)
    (test-equal (length (all-test-slot2s-bs)) 3)
    (test-assert (subsetp (list t3 t4 t5) (all-test-slot2s-bs)))
    (let ((t6 (make-instance 'test-slot4 :a 6 :b 9)))
      (test-equal (length (all-test-slots)) 6)
      (test-assert (subsetp (list t1 t2 t3 t4 t5 t6) (all-test-slots)))
      (test-equal (all-test-slot4s) (list t6)))))
      
(defclass test-class ()
  ((x :initarg :x :reader test-class-x)
   (y :initarg :y :reader test-class-y)
   (z :initarg :z :reader test-class-z))
  (:metaclass indexed-class)
  (:class-indices (2d-coords :index-type array-index :slots (x y)
			     :index-initargs (:dimensions '(256 256))
			     :index-reader test-with-2d-coords)
		  (3d-coords :index-type array-index :slots (x y z)
			     :index-reader test-with-3d-coords
			     :index-initargs (:dimensions '(256 256 2)))))

(define-indexed-class-test test-class-indices (test-class)
  (let ((t1 (make-instance 'test-class :x 1 :y 1 :z 0))
	(t2 (make-instance 'test-class :x 1 :y 3 :z 1))
	(t3 (make-instance 'test-class :x 1 :y 2 :z 0)))
    (test-equal (test-with-3d-coords '(1 1 0)) t1)
    (test-equal (test-with-2d-coords '(1 1)) t1)
    (test-equal (test-with-2d-coords '(1 2)) t3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *class-index*
    (index-create 'class-index)))

(defclass base-object ()
  ()
  (:metaclass indexed-class)
  (:class-indices (class :index *class-index*
			 :slots nil
			 :index-reader objects-of-class
			 :index-values all-objects
			 :index-subclasses t
			 :index-keys all-class-names)
		  (classes :index-type class-index
			   :index-initargs (:index-superclasses t)
			   :slots nil
			   :index-subclasses t
			   :index-reader objects-with-class)))

(defclass child1 (base-object)
  ()
  (:metaclass indexed-class))

(defclass child2 (base-object)
  ((a :initarg :a))
  (:metaclass indexed-class))

(define-indexed-class-test test-class-index (child1 child2 base-object)
  (let ((c1 (make-instance 'child1))
	(c2 (make-instance 'child1))
	(c3 (make-instance 'child1))
	(c4 (make-instance 'child2))
	(c5 (make-instance 'child2)))
    (test-equal (length (all-objects)) 5)
    (test-assert (subsetp (list c1 c2 c3 c4 c5) (all-objects)))
    (test-equal (length (objects-with-class 'child1)) 3)
    (test-assert (subsetp (list c1 c2 c3) (objects-with-class 'child1)))
    (test-equal (length (objects-with-class 'child2)) 2)
    (test-assert (subsetp (list c4 c5) (objects-with-class 'child2)))
    (test-equal (length (objects-with-class 'base-object)) 5)
    (test-assert (subsetp (list c1 c2 c3 c4 c5) (objects-with-class 'base-object)))
    (test-equal (length (objects-of-class 'child1)) 3)
    (test-assert (subsetp (list c1 c2 c3) (objects-of-class 'child1)))
    (test-equal (length (objects-of-class 'child2)) 2)
    (test-assert (subsetp (list c4 c5) (objects-of-class 'child2)))
    (test-equal (objects-of-class 'base-object) nil)
    (test-equal (length (all-class-names)) 2)
    (test-assert (member 'child1 (all-class-names)))
    (test-assert (member 'child2 (all-class-names)))))

(defclass var-test ()
  ((blorg :index-type string-unique-index
	  :initarg :blorg
	  :index-var *var-test-blorg-index*))
  (:metaclass indexed-class))

(define-indexed-class-test test-index-var (var-test)
  (let ((c1 (make-instance  'var-test :blorg "blorg")))
    (test-equal c1 (index-get *var-test-blorg-index* "blorg"))))

(defclass category-image ()
  ((category :index-type category-index
	     :index-reader images-with-category
	     :index-keys all-image-categories
	     :initarg :category
	     :reader image-category))
  (:metaclass indexed-class))

(defclass category-track ()
  ((category :index-type category-index
	     :index-initargs (:tree-test #'equal)
	     :index-reader tracks-with-category
	     :index-keys all-track-categories
	     :initarg :category
	     :reader track-category))
  (:metaclass indexed-class))

(define-indexed-class-test test-category-index (category-image category-track)
  (let ((i1 (make-instance 'category-image :category '(:photo :stills :nature)))
	(i2 (make-instance 'category-image :category '(:photo :stills :nature)))
	(i3 (make-instance 'category-image :category '(:photo :naked :woman)))
	(i4 (make-instance 'category-image :category '(:photo :naked :man)))
	(i5 (make-instance 'category-image :category '(:painting :abstract :cubist))))
    (test-equal 4 (length (images-with-category '(:photo))))
    (test-equal 2 (length (images-with-category '(:photo :stills))))
    (test-equal 2 (length (images-with-category '(:photo :stills :nature))))
    (test-equal 2 (length (images-with-category '(:photo :naked))))
    (test-equal 1 (length (images-with-category '(:photo :naked :woman))))
    (test-equal 1 (length (images-with-category '(:photo :naked :man))))
    (test-equal 0 (length (images-with-category '(:foobar))))
    (test-equal (list i4) (images-with-category '(:photo :naked :man)))
    (test-equal (list i4) (images-with-category '(:photo :naked :man)))
    (test-equal (list i5) (images-with-category '(:painting)))
    (test-equal (list i5) (images-with-category '(:painting :abstract)))
    (test-equal (list i5) (images-with-category '(:painting :abstract :cubist)))

    (test-assert (subsetp (list i1 i2 i3 i4)
			  (images-with-category '(:photo))))
    (test-assert (subsetp (list i1 i2)
			  (images-with-category '(:photo :stills :nature))))
    (test-assert (subsetp '((:photo) (:photo :stills) (:photo :stills :nature)
			    (:photo :naked) (:photo :naked :man) (:photo :naked :woman)
			    (:painting) (:painting :abstract) (:painting :abstract :cubist))
			  (all-image-categories) :test #'equal))

    (destroy-object i5)
    (test-equal 0 (length (images-with-category '(:painting))))
    (test-equal 0 (length (images-with-category '(:painting :abstract))))
    (test-equal 0 (length (images-with-category '(:painting :abstract :cubist))))

    (test-assert (subsetp '((:photo) (:photo :stills) (:photo :stills :nature)
			    (:photo :naked) (:photo :naked :man) (:photo :naked :woman))
			  (all-image-categories) :test #'equal))

    (destroy-object i4)
    (test-equal 3 (length (images-with-category '(:photo))))
    (test-equal 1 (length (images-with-category '(:photo :naked))))
    (test-equal (list i3) (images-with-category '(:photo :naked)))

    (test-assert (subsetp '((:photo) (:photo :stills) (:photo :stills :nature)
			    (:photo :naked) (:photo :naked :woman))
			  (all-image-categories) :test #'equal))))

(define-indexed-class-test test-track-category-index (category-track)
  (let ((t1 (make-instance 'category-track :category '("Rock" "Metal" "Trash")))
	(t2 (make-instance 'category-track :category '("Rock" "Metal" "Death")))
	(t3 (make-instance 'category-track :category '("Rock" "Metal" "Heavy")))
	(t4 (make-instance 'category-track :category '("Reggae" "Dub"))))
    (test-equal 3 (length (tracks-with-category '("Rock"))))
    (test-equal 3 (length (tracks-with-category '("Rock" "Metal"))))
    (test-assert (subsetp (list t1 t2 t3)
			  (tracks-with-category '("Rock"))))
    (test-assert (subsetp (list t1)
			  (tracks-with-category '("Rock" "Metal" "Trash"))))
    (test-assert (subsetp (tracks-with-category '("Rock"))
			  (tracks-with-category '("Rock" "Metal"))))
    (test-equal 1 (length (tracks-with-category '("Reggae"))))
    (test-assert (subsetp '(("Rock") ("Rock" "Metal") ("Rock" "Metal" "Death")
			    ("Rock" "Metal" "Trash") ("Rock" "Metal" "Heavy")
			    ("Reggae") ("Reggae" "Dub"))
			  (all-track-categories) :test #'equal))
    (destroy-object t1)
    (test-equal 2 (length (tracks-with-category '("Rock"))))
    (test-equal 2 (length (tracks-with-category '("Rock" "Metal"))))
    (test-assert (subsetp '(("Rock") ("Rock" "Metal") ("Rock" "Metal" "Death")
			    ("Rock" "Metal" "Heavy") ("Reggae") ("Reggae" "Dub"))
			  (all-track-categories) :test #'equal))
    (test-equal nil (tracks-with-category '("Rock" "Metal" "Trash")))))
    
   