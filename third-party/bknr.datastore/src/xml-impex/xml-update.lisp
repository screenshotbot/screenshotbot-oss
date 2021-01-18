(in-package :bknr.impex)

;;; sax parser for xml impex updater, reads updates to objects from an xml file

(defclass xml-class-updater (xml-class-importer)
  ())

(defun class-find-slot (class slot-name)
  (find-if #'(lambda (slot)
	       (equal (slot-definition-name slot) slot-name))
	   (mop:class-slots class)))

(defmethod importer-finalize ((handler xml-class-updater)
			      (instance xml-class-instance))
  (with-slots (class slots) instance
    (if (and (xml-class-unique-id-slot class)
	     (xml-class-unique-id-reader class))
	(let* ((id-slot (class-find-slot class (xml-class-unique-id-slot class)))
	       (id-value (gethash id-slot slots))
	       (obj (when id-value (funcall (xml-class-unique-id-reader class) id-value))))
	  (if (and obj id-value)
	      (progn
		(loop for slot being the hash-keys of slots using (hash-value value)
		   when (not (equal (slot-definition-name slot) (xml-class-unique-id-slot class)))
		   do
		     (format t "updating slot ~A with ~S~%" (slot-definition-name slot)
			     value)
		     (setf (slot-value obj (slot-definition-name slot))
			    value))
		obj)
	      (progn
		(warn "no id-value or object found, creating new~%")
		(call-next-method))))
	
	(call-next-method))))

(defun parse-xml-update-file (xml-file classes &key (recoder #'cxml::rod-string)
			      (importer-class 'xml-class-updater))
  (parse-xml-file xml-file classes :recoder recoder :importer-class importer-class))
