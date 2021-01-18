(in-package :cl-user)

(defpackage :bknr.statistics
  (:use :cl :cl-user)
  (:export #:make-statistics-table
	   #:note-statistic
	   #:show-statistics
	   #:clear-statistics
	   #:with-statistics-log))
