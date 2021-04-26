(in-package :util)

(defun load-countries ()
  (let* ((csv (path:catfile (util:system-source-directory :util) "./countries.csv"))
         (values (cl-csv:read-csv csv)))
    (loop for val in (cdr values)
       collect (cadr val))))

(defun load-states ()
  (let* ((csv (path:catfile (util:system-source-directory :util) "./states.csv"))
         (values (cl-csv:read-csv csv)))
    (loop for value in (cdr values)
         collect
         (format nil "~A ~A" (cadr value) (car value)))))
