(in-package :bknr.statistics)

(defclass statistics-table ()
  ((table :initform (make-hash-table))))

(defun make-statistics-table ()
  (make-instance 'statistics-table))

(defmethod note-statistic ((table statistics-table) name elapsed)
  (incf (gethash name (slot-value table 'table) 0) elapsed))

(defmethod show-statistic ((table statistics-table) name)
  (format t "~A: ~A~%" name (gethash name (slot-value table 'table))))

(defmethod show-statistics ((table statistics-table))
  (loop for name being the hash-keys of (slot-value table 'table)
	do (show-statistic table name)))

(defmethod clear-statistics ((table statistics-table))
  (setf (slot-value table 'table) (make-hash-table)))

(defmacro with-elapsed-run-time ((elapsed-var form) &body body)
  (let ((start-time (gensym)))
    `(let ((,start-time (get-internal-run-time)))
      (prog1
	  ,form
	(let ((,elapsed-var (- (get-internal-run-time) ,start-time)))
	  ,@body)))))

(defmacro with-statistics-log ((table &rest statistic-names) &rest body)
  (let ((elapsed (gensym))
	(name (gensym))
	(result (gensym)))
    `(let (,result)
      (with-elapsed-run-time (,elapsed (setq ,result (progn ,@body)))
	(dolist (,name (list ,@statistic-names))
	  (note-statistic ,table ,name ,elapsed))
	,result))))

