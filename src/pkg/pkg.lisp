(defpackage :pkg
  (:use :cl)
  (:export :define-package))
(in-package :pkg)

(defun fix-name (to from)
  (let ((to (str:split "/" (string to)))
        (from (str:split "/" (string from))))
    (intern
     (str:join
      "/"
      (cond
        ((equal ".." (car from))
         (append (butlast (butlast to))
                 (cdr from)))
        ((equal "." (car from))
         (append (butlast to) (cdr from)))
        (t
         from)))
     (symbol-package :foo))))

(defun fix-clause (name clause)
  (flet ((fix-rel (other)
           (fix-name name other)))
   (case (car clause)
     (:import-from
      `(:import-from ,(fix-rel (cadr clause))
                     ,@(cddr clause)))
     (:use-reexport
      `(:use-reexport ,@ (mapcar #'fix-rel (cdr clause))))
     (:use
      `(:use ,@ (mapcar #'fix-rel (cdr clause))))
     (otherwise clause))))

(defmacro define-package (name &rest clauses)
  `(progn
    (uiop:define-package ,name
        ,@ (loop for clause in clauses collect
                                       (fix-clause name clause)))
    (in-package ,name)))
