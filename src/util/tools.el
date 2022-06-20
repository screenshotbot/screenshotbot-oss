

(defun arnold-inspect-objects-of-type ()
  (interactive)
  (let ((type (thing-at-point 'sexp)))
    (sly-inspect (format "(reverse (bknr.datastore:class-instances '%s)))" type))))

(defun arnold-delete-bknr-object ()
  (interactive)
  (when (yes-or-no-p "Delete the BKNR object? ")
    (sly-inspector-eval "(bknr.datastore:delete-object *)")
    (message "Deleted.")))

(define-key sly-inspector-mode-map
  (kbd "<delete>") 'arnold-delete-bknr-object)
