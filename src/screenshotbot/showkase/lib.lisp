(defpackage :screenshotbot/showkase/lib
  (:use #:cl
        #:iterate
        #:util/java))
(in-package :screenshotbot/showkase/lib)

(named-readtables:in-readtable java-syntax)


(defun list-methods (class)
  (mapcar #_getName (array->list (#_getMethods class))))


(defun make-provider (module)
  (new-instance (lw-ji:find-java-class (format nil "~aCodegen" module))))

(defun get-metadata (module)
  (#_metadata (make-provider module)))

(defclass component ()
  ((name :initarg :name
         :reader name)
   (compose-component :initarg :compose-component
                      :reader compose-component)))

(defun get-components (module)
 (iter (for var in-java  (#_getComponentList (get-metadata module)))
       (collect (make-instance 'component
                               :name (#_getComponentName var)
                               :compose-component (#_getComponent var)))))

;; (get-components "com.airbnb.android.showkasesample.RootModule")
