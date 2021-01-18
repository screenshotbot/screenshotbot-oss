(in-package :shop)

;; Shopping system.

;; The shop offers both downloadable as well as mail-order type
;; products.

;; While browsing the shop web site, customers put products they want
;; to buy into a shopping cart.  The shopping cart is stored a cookie
;; in the browser.

;; Once finished with selecting products, the user is asked to either
;; log in or create a new customer object.  The amount of data that
;; the user is required to supply for the order depends on the type of
;; the order.  To buy download products, only the email address is
;; mandatory.

;; An order object is created in the database from the shopping cart
;; cookie data.

(define-persistent-class product ()
  ((price
    :update
    :type money
    :documentation
    "Price of the product, not including any taxes.")
   (name
    :update
    :type string
    :index-type string-unique-index
    :documentation
    "Short name of the product, must be unique, should be identifier")
   (description
    :update
    :type string
    :documentation
    "Textual description of the product")))

(defgeneric sell (product)
  (:documentation
   "Sell PRODUCT, adjusting the stock count if needed.  Returns the product sold."))

(defgeneric product-stock-count (product)
  (:documentation
   "Return the number of instances of PRODUCT available, or NIL if the
    product can be sold in infinite amounts.")
  (:method (product)
    "By default, assume infinite supply"
    nil))

(define-persistent-class download-product (product)
  ()
  (:documentation
   "A product that can be directly downloaded from the system.  To buy
    such a product, the customer needs to supply no shipping
    address.  Once paid, the system makes the product available to the
    customer for download."))

(define-persistent-class emailable-product (product)
  ()
  (:documentation
   "A product that can be sent to the customer by email \(i.e. an
    archive product). To buy such a product, an email address needs to
    be supplied by the customer.  Once paid, the system sends the
    order to the store personnel for fulfillment."))

(define-persistent-class mailable-product (product)
  ((stock-count :update
                :type integer
                :accessor product-stock-count
                :documentation
                "Number of instances of this product that are
                available to be sold, including reserved amounts in
                shopping carts.")
   (reserved-count :update
                   :type integer
                   :initform 0))
  (:documentation
   "A product that is sent to the customer by regular mail \(i.e. a
    t-shirt or poster).  Once paid, the system sends the order to the
    store personell for fulfillment."))

(defgeneric available-p (product count)
  (:documentation "Return a true value if COUNT units of PRODUCT are
  currently available.  Should be called with the store guard locked.")
  (:method (product count)
    (or (null (product-stock-count product))
        (<= count (product-stock-count product)))))

(defmethod product-stock-count ((product mailable-product))
  "The available stock count for a mailable product is reduced by the reserved count and returned."
  (- (slot-value product 'stock-count)
     (mailable-product-reserved-count product)))

(defmethod (setf product-stock-count) (newval (product mailable-product))
  "The available sock count for a mailable product is set to NEWVAL."
  (when (< newval (mailable-product-reserved-count product))
    (error "cannot reduce the available stock count below the reserved count"))
  (setf (slot-value product 'stock-count) newval))

(define-persistent-class shipping-address ()
  ((country :read))
  (:documentation
   "Abstract base class for shipping addresses.  The child classes of
    SHIPPING-ADDRESS implement a particular surface address structure,
    as required by the country."))

(define-persistent-class customer (user)
  ((name :update
         :documentation
         "Full name of the customer.")
   (invoice-addresses :update
                      :documentation
                      "List of invoice addresses with the preferred address being the
                      CAR of the list.")
   (shipping-addresses :update
                       :documentation
                       "List of shipping addresses with the preferred address being the
                       CAR of the list.")))

(define-persistent-class number-generator ()
  ((name :read
         :type symbol
         :initform (error "cannot make number-generator instance without name")
         :index-type string-unique-index
         :index-reader number-generator-with-name)
   (next :update
         :type integer
         :initarg :next
         :initform 1)))

(defun get-next-number (name)
  (with-transaction (:get-next-number)
    (let* ((number-generator (or (number-generator-with-name name)
                                 (make-instance 'number-generator :name name)))
           (number (number-generator-next number-generator)))
      (incf (number-generator-next number-generator))
      number)))

(define-persistent-class order ()
  ((number :read
           :initform (get-next-number 'orders))
   (customer :read)
   (items :update)))

(defgeneric make-order (customer shopping-cart)
  (:documentation
   "Create a new ORDER instance, initialized from the CUSTOMER and
    SHOPPING-CART objects supplied.  Returns the order created."))

(define-persistent-class invoice ()
  ((number :read
           :initform (get-next-number 'invoices))
   (items :update)))

(define-persistent-class lease ()
  ((product :read
            :initform (error "missing :product initarg to lease creation")
            :documentation "product that has been leased")
   (count :read
          :initform (error "missing :count initarg to lease creation")
          :documentation "number of units of product held by this lease")
   (fulfilled :update
              :initform nil
              :documentation "Set to a true value when the lease has
              been fulfilled.  Used during lease descruction in order
              to determine whether to return the leased inventory to
              the product stock."))
  (:documentation "Instance representing a lease for a product."))

(defgeneric update-reserved-stock (product count)
  (:documentation "Update the reserved counter of PRODUCT by COUNT units")
  (:method (product count)
    (declare (ignore product count))))

(defgeneric note-sale (product count)
  (:documentation "Update the stock count of the PRODUCT by COUNT
  units after a sale has been done")
  (:method (product count)
    (declare (ignore product count))))

(defmethod initialize-instance :after ((lease lease) &key)
  (update-reserved-stock (lease-product lease) (lease-count lease)))

(defmethod destroy-object :before ((lease lease))
  (unless (lease-fulfilled lease)
    (update-reserved-stock (lease-product lease) (- (lease-count lease)))))

(defmethod update-reserved-stock ((product mailable-product) count)
  (incf (mailable-product-reserved-count product) count))

(defmethod note-sale ((product mailable-product) count)
  (decf (slot-value product 'stock-count) count)
  (update-reserved-stock product (- count)))

(define-persistent-class shopping-cart ()
  ((leases :update
           :initform nil)
   (expires :read
            :initform (error "missing :expires initarg to shopping cart creation")
            :documentation "universal time at which this shopping cart expires"))
  (:documentation "Represents the intent to buy goods, in the form of LEASE objects"))

(defmethod destroy-object :before ((shopping-cart shopping-cart))
  (mapc #'delete-object (shopping-cart-leases shopping-cart)))

(define-condition insufficient-inventory (error)
  ((product :initarg :product
            :reader product)
   (requested :initarg :requested
              :reader requested)
   (available :initarg :available
              :reader available))
  (:report (lambda (c stream)
             (format stream "Insufficient inventory for product ~A - Requested ~A, but~[~; only~]~:* ~A available"
                     (product c) (requested c) (available c))
             c)))

(define-condition product-already-in-shopping-cart (error)
  ((product :initarg :product
            :reader product))
  (:report (lambda (c stream)
             (format stream "Product ~A is already in shopping cart"
                     (product c)))))

(defun put-to-shopping-cart (count product shopping-cart)
  "Reserve COUNT units of PRODUCT, signalling a INSUFFICIENT-INVENTORY
  error if not enough inventory of PRODUCT is available.  Returns a
  LEASE object."
  (with-store-guard ()
    (unless (available-p product count)
      (error 'insufficient-inventory
             :product product
             :requested count
             :available (product-stock-count product)))
    (when (find product (shopping-cart-leases shopping-cart)
                :key #'lease-product)
      (error 'product-already-in-shopping-cart
             :product product))
    (with-transaction (:make-lease)
      (push (make-instance 'lease
                           :product product
                           :count count)
            (shopping-cart-leases shopping-cart)))))

(defun fulfill (shopping-cart)
  "Fulfill the given shopping cart."
  (with-transaction (:fulfill)
    (dolist (lease (shopping-cart-leases shopping-cart))
      (let ((product (lease-product lease))
            (count (lease-count lease)))
        (setf (lease-fulfilled lease) t)
        (note-sale product count)))
    (delete-object shopping-cart)))

;;; TESTING

(defun getpid ()
  #+openmcl
  (ccl::getpid)
  #+sbcl
  (sb-posix:getpid)
  #+(not (or sbcl openmcl))
  (random 10000))

(defmacro with-temporary-directory ((pathname) &body body)
  `(let ((,pathname (pathname (format nil "/tmp/store-test-~A/" (getpid)))))
     (asdf:run-shell-command "rm -rf ~A" ,pathname)
     (prog1
         (progn ,@body)
       (asdf:run-shell-command "rm -rf ~A" ,pathname))))

(defun do-with-test-store (thunk)
  (when (and (boundp '*store*) *store*)
    (warn "closing open store *store* to run tests")
    (close-store))
  (with-temporary-directory (store-directory)
    (make-instance 'mp-store
                   :subsystems (list (make-instance 'store-object-subsystem))
                   :directory store-directory)
    (funcall thunk)
    (close-store)))

(defmacro with-test-store (() &body body)
  `(do-with-test-store (lambda () ,@body)))

(unit-test:deftest :shop "lease and cart tests"
  (with-test-store ()
    (let* ((t-shirt (make-instance 'mailable-product :name 't-shirt :stock-count 10))
           (file (make-instance 'download-product :name 'file))
           (shopping-cart (make-instance 'shopping-cart :expires (+ (* 60 10) (get-universal-time)))))
      (unit-test:test-equal 10 (product-stock-count t-shirt))
      (put-to-shopping-cart 10 t-shirt shopping-cart)
      (unit-test:test-equal 0 (product-stock-count t-shirt))
      (unit-test:test-assert (product-stock-count t-shirt))
      (with-transaction (:add-to-inventory)
        (incf (slot-value t-shirt 'stock-count) 10))
      (put-to-shopping-cart 5 t-shirt shopping-cart)
      (unit-test:test-equal 5 (product-stock-count t-shirt))
      (delete-object shopping-cart)
      (unit-test:test-equal 20 (product-stock-count t-shirt))
      (setf shopping-cart (make-instance 'shopping-cart :expires (+ (* 60 10) (get-universal-time))))
      (put-to-shopping-cart 5 t-shirt shopping-cart)
      (put-to-shopping-cart 500 file shopping-cart)
      (unit-test:test-equal 15 (product-stock-count t-shirt)))))

(unit-test:deftest :shop "fulfill test"
  (with-test-store ()
    (let* ((t-shirt (make-instance 'mailable-product :name 't-shirt :stock-count 10))
           (file (make-instance 'download-product :name 'file))
           (shopping-cart (make-instance 'shopping-cart :expires (+ (* 60 10) (get-universal-time)))))
      (put-to-shopping-cart 3 t-shirt shopping-cart)
      (put-to-shopping-cart 7 file shopping-cart)
      (fulfill shopping-cart)
      (unit-test:test-equal 7 (product-stock-count t-shirt))
      (unit-test:test-equal 0 (length (class-instances 'shopping-cart)))
      (unit-test:test-equal 0 (length (class-instances 'lease)))
      (unit-test:test-equal 0 (mailable-product-reserved-count t-shirt)))))