(uiop:define-package #:markup/walk
    (:use #:cl)
  (:export #:walk
           #:add-attrs)
  (:import-from #:markup/markup
                #:unescaped-string
                #:escaped-string
                #:make-xml-tag
                #:make-xml-merge-tag
                #:xml-merge-tag
                #:xml-tag-name
                #:xml-merge-tag-children
                #:xml-tag-children
                #:abstract-xml-tag
                #:xml-tag-attributes))
(in-package #:markup/walk)

(defgeneric walk (tree fn)
  (:documentation "Walk the tree, giving you the option to transform
  each element"))

(defmethod walk (tree fn)
  ;; do nothin
  (error "unexpected ~S" tree))

(defmethod walk ((tree string) fn)
  tree)

(defmethod walk ((tree unescaped-string) fn)
  tree)

(defmethod walk ((tree escaped-string) fn)
  tree)

(Defmethod walk ((tree xml-merge-tag) fn)
  (make-xml-merge-tag
   :children (loop for child in (xml-merge-tag-children tree) collect
                  (walk child fn))))

(defmethod walk ((tree list) fn)
  (loop for x in tree collect
       (walk x fn)))

(Defmethod walk ((tree abstract-xml-tag) fn)
  (let ((ret (funcall fn tree)))
    (make-xml-tag (xml-tag-name ret)
                  :attributes (xml-tag-attributes ret)
                  :children (walk (xml-tag-children ret) fn))))

(defmethod add-attrs ((tag abstract-xml-tag) &rest args &key &allow-other-keys)
  (let ((attr (xml-tag-attributes tag)))
    (dolist (item (alexandria:plist-alist args))
      (destructuring-bind (key . value) item
        (setf (alexandria:assoc-value attr (string-downcase (string key))) value)))
    (make-xml-tag (xml-tag-name tag)
                  :attributes attr
                  :children (xml-tag-children tag))))
