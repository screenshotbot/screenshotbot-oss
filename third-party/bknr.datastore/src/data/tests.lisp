;; hier mal unit tests reintun, im moment nur rumgeteste
(in-package :bknr.datastore)

(define-persistent-class boeses-object ()
  ((a :read)))

(define-persistent-class foobar ()
  ((a :read :index-type unique-index :index-values all-foobars)))

(deftransaction boese-tx ()
  (format t "BOESE BOESE!~%"))

(defmethod initialize-transient-instance ((boese boeses-object))
  (boese-tx))

(make-instance 'mp-store :directory "/tmp/spackstore/"
                         :subsystems (list (make-instance 'store-object-subsystem)))

(defmethod reinitialize-instance :around ((class persistent-class) &rest args)
  (declare (ignore args))
  (format t "reinit~%")
  (call-next-method))

;;; muesste fehler werfen weil transaction in initialize-transient-instance
(make-instance 'boeses-object)

(make-instance 'foobar :a 2)

;;; jetzt foobar bitte nicht mehr persistent
(defclass foobar ()
  ((a :initarg :a :reader foobar-a)))

;;; jetzt muesste foobar ID:2 nicht mehr in all-store-objects drin
;;; sein, soll das destroyed werden?

(define-persistent-class foobar ()
  ((a :read :index-type unique-index :index-values all-foobars)))

(define-persistent-class foobar ()
  ((a :read :index-type unique-index :index-values all-foobars)
   (b :read :initform t)))
