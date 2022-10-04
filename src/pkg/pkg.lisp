(defpackage :pkg
  (:use :cl)
  (:export :define-package))
(in-package :pkg)

(defun fix-name (to from)
  (let ((to (str:split "/" (string to)))
        (from (str:split "/" (string from))))
    (make-symbol
     (str:join
      "/"
      (cond
        ((equal ".." (car from))
         (append (butlast (butlast to))
                 (cdr from)))
        ((equal "." (car from))
         (append (butlast to) (cdr from)))
        (t
         from))))))

(defun fix-clause (name clause)
  (labels ((fix-rel (other)
           (fix-name name other))
           (fix-import-from (clause)
             `(,(car clause) ,(fix-rel (cadr clause))
               ,@(cddr clause))))
   (case (car clause)
     (:import-from
      (fix-import-from clause))
     (:use-reexport
      `(:use-reexport ,@ (cdr clause)))
     (:use
      `(:use ,@ (mapcar #'fix-rel (cdr clause))))
     (:shadowing-import-from
      clause)
     (otherwise clause))))

(defmacro define-package (name &rest clauses)
  `(progn
    (uiop:define-package ,name
        ,@ (loop for clause in clauses collect
                                       (fix-clause name clause)))
    (in-package ,name)))
