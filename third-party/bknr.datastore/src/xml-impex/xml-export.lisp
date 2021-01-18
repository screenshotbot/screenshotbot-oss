(in-package :bknr.impex)

(defmethod slot-serialize-value ((slot xml-effective-slot-definition) value)
  (with-slots (serializer object-to-id) slot
    (when object-to-id
      (setf value (funcall object-to-id value)))
    (when serializer
      (setf value (funcall serializer value)))))

(defvar *objects-written*)

(defmacro write-to-xml (object &key name (output '*standard-output*) (indentation 3) (canonical nil))
  `(let ((*objects-written* (make-hash-table :test #'equal))
         (cxml::*current-element* nil)
         (sink (cxml:make-character-stream-sink ,output :indentation ,indentation :canonical ,canonical)))
     (with-xml-output sink
       (build-xml ,object :name ,name))))

(defmacro write-to-xml-string (object &key name (indentation 3) (canonical nil))
  `(let ((output-string (make-string-output-stream)))
     (get-output-stream-string
      (write-to-xml ,object :name ,name :output output-string :indentation ,indentation :canonical ,canonical))))

(defmacro with-xml-export* ((&key output indentation canonical) &body body)
  `(let ((*objects-written* (make-hash-table :test #'equal))
         (cxml::*current-element* nil)
         (cxml::*sink* (cxml:make-character-stream-sink ,output :indentation ,indentation :canonical ,canonical)))
     (with-xml-output cxml::*sink*
       ,@body)))

(defmacro with-xml-export (nil &body body)
  `(with-xml-export* (:output *standard-output* :indentation 3 :canonical nil)
     ,@body))

(defun write-object-reference (class object unique-id-slot-name name)
  (let ((slotdef (find unique-id-slot-name (class-slots class) :key #'slot-definition-name)))
    (unless (xml-effective-slot-definition-attribute slotdef)
      (error "Slot ~A is not defined as :attribute slot and cannot be used as unique-id slot for class ~A" unique-id-slot-name (class-name class)))
    (sax:start-element cxml::*sink* nil nil name
                       (list (sax:make-attribute :qname (cxml::string-rod (xml-effective-slot-definition-attribute slotdef))
                                                 :value (cxml::string-rod (slot-serialize-value slotdef (slot-value object unique-id-slot-name))))))
    (sax:end-element cxml::*sink* nil nil name)))

(defgeneric build-xml (object &key)
  (:documentation "Write OBJECT to XML stream")


  (:method ((object (eql nil)) &key))

  (:method ((object list) &key (name (error "Can not serialize list to XML without an element name~%")))
      (sax:start-element cxml::*sink* nil nil (cxml::string-rod name) nil)
      (dolist (obj object)
        (build-xml obj))
      (sax:end-element cxml::*sink* nil nil (cxml::string-rod name)))
  
  (:method ((object string) &key (name (error "Can not serialize string ~A to XML without an element name." object)))
      (sax:start-element cxml::*sink* nil nil (cxml::string-rod name) nil)
      (sax:characters cxml::*sink* (cxml::string-rod object))
      (sax:end-element cxml::*sink* nil nil (cxml::string-rod name)))
  
  (:method ((object standard-object) &key name)
    (if (typep (class-of object) 'xml-class)
        (progn
          (xml-object-check-validity object)
          (let* ((class (class-of object))
                 (qname (cxml::string-rod (or name (xml-class-element class)))))
            ;; If this object has been serialized to the XML stream,
            ;; write a reference to the object and return.
            (with-slots (unique-id-slot) class
              (when unique-id-slot
                (if (gethash (slot-value object (first unique-id-slot)) *objects-written*)
                    (progn
                      (write-object-reference class object (first unique-id-slot) qname)
                      (return-from build-xml))
                    (setf (gethash (slot-value object (first unique-id-slot)) *objects-written*) t))))
            
            ;; Object has not been written to the XML file or no
            ;; unique-id-slot is defined for this class.
            (let* ((attr-slots (xml-class-attribute-slots class))
                   (elt-slots (xml-class-element-slots class))
                   (body-slot (xml-class-body-slot class))
                   ;; attributes
                   (attributes (loop for slot in attr-slots
                                  for name = (slot-definition-name slot)
                                  for attdef = (cxml::string-rod (xml-effective-slot-definition-attribute slot))
                                  when (and (slot-boundp object name)
                                            (slot-value object name))
                                  collect (sax:make-attribute
                                           :qname attdef
                                           :value
                                           (cxml::string-rod
                                            (slot-serialize-value slot (slot-value object name)))))))
              (sax:start-element cxml::*sink* nil nil qname attributes)
              
              ;; elements
              (dolist (slot elt-slots)
                (let ((name (slot-definition-name slot))
                      (element-name (xml-effective-slot-definition-element slot)))
                  (when (slot-boundp object name)
                    (if (consp (slot-value object name))
                        (dolist (child (slot-value object name))
                          (if (typep (class-of child) 'xml-class)
                              (build-xml child)
                              (build-xml (slot-serialize-value slot child) :name element-name)))
                  (let ((child (slot-value object name)))
                    (if (typep (class-of child) 'xml-class)
                        (build-xml child)
                        (build-xml (slot-serialize-value slot child) :name element-name)))))))
              
              ;; body slot
              (when body-slot
                (let ((name (slot-definition-name body-slot)))
                  (when (slot-boundp object name)
                    (sax:characters
                     cxml::*sink*
                     (cxml::string-rod
                      (funcall (xml-effective-slot-definition-serializer body-slot)
                               (slot-value object name)))))))
        
              (sax:end-element cxml::*sink* nil nil qname))))
        (cxml:with-element (string-downcase (class-name (class-of object)))
          (dolist (slot (class-slots (class-of object)))
            (cxml:with-element (string-downcase (symbol-name (slot-definition-name slot)))
              (let ((value (slot-value object (slot-definition-name slot))))
                (when value
                  (cxml:text (handler-case
                                 (cxml::utf8-string-to-rod (princ-to-string value))
                               (error (e)
                                 (declare (ignore e))
                                 (cxml::utf8-string-to-rod "[unprintable]"))))))))))))
