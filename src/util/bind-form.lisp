(in-package :util)

(markup:deftag bind-form (children &key data name)
  (declare (debug 3))
  (setf children (remove-if 'stringp children))

  (unless data
    (return-from bind-form (markup:make-merge-tag children)))

  (loop for x being the hash-keys of data do
       (unless (stringp x)
         (error "expected string hash keys got ~S" x)))
  (markup:make-merge-tag
   (markup:walk children
                (lambda (xml-tag)
                  (cond
                    ((not xml-tag) (error "shouldn't have seen nil here"))
                    ((equal :input (markup:xml-tag-name xml-tag))
                     (symbol-macrolet ((name-attr  (assoc-value (markup:xml-tag-attributes xml-tag) "name" :test 'equal))
                                       (value-attr (assoc-value (markup:xml-tag-attributes xml-tag) "value" :test 'equal)))


                       (multiple-value-bind (res parts)
                           (cl-ppcre:scan-to-strings "^(.*)[{](.*)[}]$" name-attr)
                         (cond
                           ((and res
                                 (equal name (aref parts 0)))
                            (let ((new-val (gethash (aref parts 1) data)))
                              (setf value-attr new-val))
))))
                     xml-tag)
                    (t
                     xml-tag))))))
