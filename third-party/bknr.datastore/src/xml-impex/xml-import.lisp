(in-package :bknr.impex)

(defclass xml-class-importer (sax:default-handler)
  ((dtd        :initarg :dtd :initform nil :reader importer-dtd)
   (class-hash :initarg :class-hash :accessor importer-class-hash)
   (root-elt   :initform nil :accessor importer-root-elt)
   (parent-elts :initform nil :accessor importer-parent-elts)))

(defmethod slot-parse-value ((slot xml-effective-slot-definition) value)
  (with-slots (parser id-to-object) slot
    (when parser
      (setf value (funcall parser value)))
    (when id-to-object
      (setf value (funcall id-to-object value)))
    value))

;;; description for an object instance to be created from the xml. The
;;; data is gathered while parsing the XML, and at the end of an
;;; element, the corresponding object is instanciated.

(defclass xml-node ()
  ((element :initarg :element :accessor node-element)
   (children :initarg :children :initform (make-hash-table) :accessor node-children)
   (elmdef    :initarg :elmdef :accessor instance-elmdef)
   (attributes :initarg :attributes :accessor node-attributes)
   (data :initarg :data :initform nil :accessor node-data)))

(defmethod print-object ((node xml-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~a" (node-element node))))

(defclass xml-class-instance (xml-node)
  ((slots :initform (make-hash-table :test #'equal) :accessor instance-slots)
   (class     :initarg :class :initform nil :accessor instance-class)))

(defmethod print-object ((instance xml-class-instance) stream)
  (print-unreadable-object (instance stream :type t)
    (format stream "~a" (instance-class instance))))

(defgeneric importer-add-attribute (handler node attr))
(defgeneric importer-add-characters (handler node data))
(defgeneric importer-add-element (handler node element value))
(defgeneric importer-finalize (handler node))

(defmethod importer-add-attribute ((handler xml-class-importer)
                                 (class-instance xml-class-instance) attr)
  (with-slots (class slots) class-instance
    (let ((slot (xml-class-find-attribute-slot class (sax:attribute-qname attr))))
      (when slot
        (setf (gethash slot slots) (slot-parse-value slot (sax:attribute-value attr)))))))

(defmethod importer-add-attribute ((handler xml-class-importer)
                                 (node xml-node) attr)
  nil)

(defmethod importer-add-characters ((handler xml-class-importer)
                                  (node xml-node) characters)
  (unless (whitespace-p characters)
    (setf characters (string-trim bknr.utils::+whitespace-chars+ characters))
    (with-slots (data) node
      (setf data (if data
                     (concatenate 'string data characters)
                     characters)))))

(defmethod importer-add-characters ((handler xml-class-importer)
                                  (instance xml-class-instance) characters)
  (with-slots (class elmdef slots children) instance
    (let ((slot (xml-class-body-slot class)))
      (when slot
        (setf (gethash slot slots)
              (concatenate 'string
                           (gethash slot slots)
                           (slot-parse-value slot characters)))))))

(defmethod importer-add-element ((handler xml-class-importer)
                               (node xml-node) element value)
  (with-slots (children) node
    (push value (gethash (make-keyword-from-string element) children))))

(defmethod importer-add-element ((handler xml-class-importer)
                               (instance xml-class-instance) element value)
  (with-slots (slots elmdef class children) instance
    (let ((slot (xml-class-find-element-slot class element)))
      (when slot
          ;; parse the value if necessary
          (setf value (slot-parse-value slot value))
          (let ((containment (xml-effective-slot-definition-containment slot)))
            (if (member containment '(:* :+))
                ;; if it has a plural containment, push the
                ;; created instance into the initargs hash
                (push value (gethash slot slots))
                ;; else set the initarg hash to the new instance
                (setf (gethash slot slots) value)))))))

(defmethod importer-finalize ((handler xml-class-importer)
                              (node xml-node))
  (with-slots (data children) node
    (cond
      ((and data
            (= (hash-table-count children) 0))
       data)
      ((> (hash-table-count children) 0)
       (children-to-initforms children))
      (t nil))))

(defun add-parent (handler parent child)
  (let* ((class (class-of child))
         (parent-slot (when (typep class 'xml-class)
                        (xml-class-parent-slot class))))
    (when parent-slot
      (set-slot-value handler child (slot-definition-name parent-slot) parent))))

(defun slots-to-initforms (slots)
  (let (initforms)
    (loop for slot being the hash-keys of slots using (hash-value value)
       when (listp value)
       do (push (reverse value) initforms)
       else do (push value initforms)
       do (push (first (slot-definition-initargs slot)) initforms))
    initforms))

(defun children-to-initforms (children)
  (let (initforms)
    (loop for child being the hash-keys of children using (hash-value value)
       when (listp value)
       do (push (reverse value) initforms)
       else do (push value initforms)
       do (push child initforms))
    initforms))

(defmethod importer-finalize ((handler xml-class-importer)
                            (instance xml-class-instance))
  (with-slots (class elmdef children slots) instance
    (let* ((initforms (slots-to-initforms slots))
           (object (apply #'create-instance handler (class-name class) initforms)))

      (loop for objs being the hash-values of slots
         when (listp objs)
         do (dolist (child objs)
              (add-parent handler object child))
         else do (add-parent handler object objs))

      object)))

(defmethod sax:start-document ((handler xml-class-importer))
  (setf (importer-root-elt handler) nil))

(defmethod sax:start-element ((handler xml-class-importer) namespace-uri local-name qname attrs)
  (declare (ignore namespace-uri local-name))
  (let ((class (gethash qname (importer-class-hash handler)))
        (element (cxml::string-rod qname))
        instance)
    (if class
        (setf instance
              (make-instance 'xml-class-instance
                             :element element
                             :elmdef (xml-class-element class)
                             :class class))
        (setf instance
              (make-instance 'xml-node
                             :element element
                             :elmdef (cxml::find-element element (importer-dtd handler)))))

    (dolist (attr attrs)
      (importer-add-attribute handler instance attr))
    
    (push instance (importer-parent-elts handler))))

(defmethod sax:characters ((handler xml-class-importer) data)
  (unless (importer-parent-elts handler)
    (error "Can not parse SAX:CHARACTERS without a parent element."))
  (importer-add-characters handler (first (importer-parent-elts handler)) data))

(defmethod create-instance ((handler xml-class-importer) class-name &rest initargs)
  (apply #'make-instance class-name initargs))

(defmethod set-slot-value ((handler xml-class-importer) object slot-name value)
  (setf (slot-value object slot-name) value))

(defmethod sax:end-element ((handler xml-class-importer) namespace-uri local-name qname)
  (declare (ignore namespace-uri local-name))

  (let* ((instance (pop (importer-parent-elts handler)))
         (final (importer-finalize handler instance))
         (parent (first (importer-parent-elts handler))))

    (when parent
      (importer-add-element handler parent qname final))
    
    (setf (importer-root-elt handler) final)))

(defun parse-xml-file (xml-file classes &key (recoder #'cxml::rod-string)
                       (importer-class 'xml-class-importer))
  (let ((class-hash (make-hash-table :test #'equal)))
    (dolist (class classes)
      (setf (gethash (xml-class-element class) class-hash) class))
    (let ((importer (make-instance importer-class
                                   :dtd (parse-dtd-file (xml-class-dtd-name (first classes)))
                                   :class-hash class-hash)))
      (cxml:parse-file xml-file (cxml:make-recoder importer recoder))
      (importer-root-elt importer))))
