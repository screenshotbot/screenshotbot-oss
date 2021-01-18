(in-package :shop)

(defclass money ()
  ((currency :initarg :currency
             :reader currency
             :type symbol)
   (amount :initarg :amount
           :accessor amount
           :type integer))
  (:documentation
   "Represent monetary value, including a currency."))

(defun make-money (amount &optional (currency :eur))
  (make-instance 'money :currency currency :amount amount))

(defmethod print-object ((money money) stream)
  (print-unreadable-object (money stream :type t)
    (format stream "~D.~2,'0D ~A"
            (floor (amount money) 100)
            (mod (amount money) 100)
            (currency money))))

(defmethod bknr.datastore::encode-object ((money money) stream)
  (bknr.datastore::%write-char #\C stream)
  (bknr.datastore::%encode-symbol (currency money) stream)
  (bknr.datastore::%encode-integer (amount money) stream))

(defmethod bknr.datastore::decode-object ((tag (eql #\C)) stream)
  (make-instance 'money
                 :currency (bknr.datastore::%decode-symbol stream)
                 :amount (bknr.datastore::%decode-integer stream)))
