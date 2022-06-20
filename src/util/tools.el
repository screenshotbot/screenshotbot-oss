

(defun arnold-inspect-objects-of-type ()
  (interactive)
  (let ((type (thing-at-point 'sexp)))
    (sly-inspect (format "(reverse (bknr.datastore:class-instances '%s)))" type))))
