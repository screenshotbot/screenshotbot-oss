(in-package :cl-user)

;;; general helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro awhen (test-form &rest then-forms)
  `(let ((it ,test-form))
     (when it ,@then-forms)))

(defmacro aif (pred then-form &optional else-form)
  `(let ((it ,pred)) (if it ,then-form ,else-form)))

(defun string-null (string)
  (string-equal string ""))

(defconstant +whitespace-chars+
  '(#\Space #\Newline #\Tab #\Linefeed))

(defun whitespace-char-p (c)
  (member c +whitespace-chars+))

(defun whitespace-p (c-or-s)
  (cond ((stringp c-or-s)
	 (every #'whitespace-char-p c-or-s))
	((characterp c-or-s)
	 (whitespace-char-p c-or-s))
	(t nil)))

(defun make-keyword-from-string (string)
  (if (keywordp string)
      string
      (nth-value 0 (intern (string-upcase
			    (substitute-if #\- #'(lambda (char)
						   (or (whitespace-char-p char)
						       (eql #\: char)))
					   string)) 'keyword))))


;;; cxml helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun child-elements (node)
  (coerce (remove-if-not #'dom:element-p (dom:child-nodes node)) 'list))

(defmacro with-attributes (attributes node &rest body)
  `(let ,(loop for attr in attributes
	       when (symbolp attr)
	       collect `(,attr (dom:get-attribute ,node ,(string-downcase (symbol-name attr))))
	       when (listp attr)
	       collect `(,(car attr) (dom:get-attribute ,node ,(cadr attr))))
    ,@(loop for attr in attributes
	    when (symbolp attr)
	    collect `(when (string-null ,attr)
		      (error ,(format nil "Attribute ~S is empty."
				      (string-downcase (symbol-name attr)))))
	    when (listp attr)
	    collect `(when (string-null ,(car attr))
		      (error ,(format nil "Attribute ~S is empty." (cadr attr)))))
    ,@body))


;;; xml schema parser ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; element and attribute environment

(defvar *xml-schema-envs* nil
  "This special variables holds the list of the current xml schema
element definition environments. Environments can be nested, the list
holds them in top to bottom order (the toplevel environment is first.")

(defun get-xml-schema-ref (ref)
  "Get the definition of REF from the current environment stack."
  (dolist (env *xml-schema-envs*)
    (awhen (gethash ref env)
	   (return it))))

(defun (setf get-xml-schema-ref) (newvalue ref)
  "Set the definition of REF in the current environment."
  (let ((env (first *xml-schema-envs*)))
    (awhen (gethash ref env)
      (error "There already is an XML Schema element named ~A: ~A." ref it))
    (setf (gethash ref env) newvalue)))

;;; xml schema types

(defgeneric parse-xs-type (type elt)
  (:documentation "Parse ELT according to TYPE. TYPE can be a keyword
to identify base datatypes, or a class derived from XS-TYPE."))

(defmacro define-xs-type (name (elt) &rest body)
  "Define a base XML Schema type, named by a keyword. For example,
\"xs:string\" is identified by :XS-STRING."
  (let ((n (gensym)))
    `(defmethod parse-xs-type (,(if (keywordp name)
				    `(,n (eql ,name))
				    name)
			       ,elt)
      ,@body)))

(defmacro define-xs-type-error (name (elt) &rest body)
  "Define the default error function called when ELT could not be
parsed as a value of type NAME."
  `(define-xs-type ,name ((,elt t))
    ,@body))

;;; Einfach XML Schema typen, wie primitive Types, einfach Elements
;;; und Attributes werden direkt zu Lisp primitive geparst. 

(define-xs-type :xs-string ((elt dom-impl::text))
    (dom:node-value elt))

(define-xs-type :xs-string ((elt dom-impl::node))
    (let ((children (dom:child-nodes elt)))
      (if (and (= (length children) 1)
	       (dom:text-node-p (aref children 0)))
	  (dom:node-value (aref children 0))
	  "")))

(define-xs-type-error :xs-string (elt)
  (error "~s could not be parsed as xs:string." elt))

(defclass xs-elt ()
  ((name :initarg :name :initform nil :reader xs-elt-name)
   (type :initarg :type :initform nil :reader xs-elt-type)))

(defun create-xs-elt (node)
  (unless (= (length (dom:child-nodes node)) 0)
    (error "~a is not a simple XML Scheme element node." node))
  (with-attributes (name type) node
    (setf (get-xml-schema-ref name)
	  (make-instance 'xs-elt
			 :name name
			 :type (make-keyword-from-string type)))))

(defclass xs-attribute (xs-elt)
  ())

(defun create-xs-attribute (node)
  (unless (= (length (dom:child-nodes node)) 0)
    (error "~a is not an XML Scheme attribute node." node))
  (with-attributes (name type) node
    (setf (get-xml-schema-ref name)
	  (make-instance 'xs-attribute
			 :name name
			 :type (make-keyword-from-string type)))))

(define-xs-type (type xs-elt) (elt)
  (parse-xs-type (xs-elt-type type) elt))


(defclass xs-complex-type (xs-type)
  ((attrs :initarg :attrs :reader xs-ctype-attrs)
   (children :initarg :children :reader xs-ctype-children)
   (content :initarg :content :reader xs-ctype-content)))


(defclass xs-element ()
  ((name :initarg :name :reader xs-type-name)
   (type :initarg :type :reader xs-type-type)))

(defun xs-attribute-p (node)
  (string-equal (dom:node-name node) "xs:attribute"))

(defun xs-element-p (node)
  (string-equal (dom:node-name node) "xs:element"))

(defun xs-simple-type-p (node)
  (or (xs-attribute-p node)
      (and (xs-element-p node)
	   (null (child-elements node)))))

(defun xs-complex-type-p (node)
  (let ((children (child-elements node)))
    (and (xs-element-p node)
	 (not (null children))
	 (let ((child (first children)))
	   (string-equal (dom:node-name node)
			 "xs:complexType")))))

(defun parse-schema-node (elt)
  (cond ((xs-attribute-p elt)
	 (create-xs-attribute elt))
	((xs-simple-type-p elt)
	 (create-xs-simple-type elt))
	#+nil
	((xs-complex-type-p elt)
	 (create-xs-complex-type elt))
	(t (error "Unknown top-level XML Schema node: ~A." (dom:node-name elt)))))

(defun parse-schema-file (filename)
  "Returns the toplevel XML schema environment."
  (let* ((dom (cxml:parse-file filename (dom:make-dom-builder)))
	 (root (dom:document-element dom))
	 (*xml-schema-envs* (list (make-hash-table))))
    (unless (string-equal (dom:node-name root) "xs:schema")
      (error "Document is not an XML Schema document."))
    (dolist (elt (child-elements root))
      (parse-schema-node elt))
    (pop *xml-schema-envs*)))