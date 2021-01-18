(in-package :bknr.datastore)

(defclass persistent-xml-class (bknr.impex::xml-class
                                bknr.datastore::persistent-class)
  ())

(defmethod validate-superclass ((sub persistent-xml-class) (super bknr.impex::xml-class))
  t)

(defmethod validate-superclass ((sub persistent-xml-class)
                                (super bknr.datastore::persistent-class))
  t)

(defclass persistent-xml-direct-slot-definition
    (bknr.impex::xml-direct-slot-definition
     bknr.datastore::persistent-direct-slot-definition)
  ())

(defclass persistent-xml-effective-slot-definition
    (bknr.impex::xml-effective-slot-definition
     bknr.datastore::persistent-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class persistent-xml-class) &key &allow-other-keys)
  'persistent-xml-direct-slot-definition)

(defmethod effective-slot-definition-class ((class persistent-xml-class)
                                            &key &allow-other-keys)
  'persistent-xml-effective-slot-definition)

(defclass persistent-xml-class-importer (bknr.impex:xml-class-importer)
  ())

(defmethod bknr.impex::create-instance ((importer persistent-xml-class-importer) class-name &rest initforms)
  (apply #'make-instance class-name initforms))

(defmethod bknr.impex::set-slot-value ((handler persistent-xml-class-importer) object slot-name value)
  (change-slot-values object slot-name value))

(export '(persistent-xml-class))
