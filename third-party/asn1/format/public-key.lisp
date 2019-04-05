(defpackage #:asn1/format/public-key
  (:use #:cl)
  (:import-from #:optima.core
                #:%assoc
                #:constructor-pattern
                #:make-destructor
                #:constructor-pattern-subpatterns
                #:subpatterns
                #:constructor-pattern-destructor-sharable-p
                #:constructor-pattern-make-destructor 
                #:parse-constructor-pattern
                #:parse-pattern
                #:unparse-pattern)
  (:export #:subject-public-key-info
           #:algorithm-identifier
           #:subject-public-key))
(in-package #:asn1/format/public-key)

(defstruct (subject-public-key-info (:include constructor-pattern)
                                    (:constructor make-subject-public-key-info (algorithm-pattern subject-public-key
                                                                                &aux (subpatterns
                                                                                      (list algorithm-pattern
                                                                                            subject-public-key))))))

(defmethod constructor-pattern-destructor-sharable-p ((x subject-public-key-info) (y subject-public-key-info))
  t)

(defmethod constructor-pattern-make-destructor ((pattern subject-public-key-info) var)
  (make-destructor :predicate-form `(and (consp ,var)
                                         (consp (car ,var))
                                         (null (cdr ,var))
                                         (eq (car (car ,var)) :sequence)
                                         (consp (cdr (car ,var))))
                   :accessor-forms (list `(list (first (cdr (car ,var))))
                                         `(list (second (cdr (car ,var)))))))

(defmethod parse-constructor-pattern ((name (eql 'subject-public-key-info)) &rest args)
  (apply #'make-subject-public-key-info (mapcar #'parse-pattern args)))

(defmethod unparse-pattern ((pattern subject-public-key-info))
  (destructuring-bind (alg key)
      (constructor-pattern-subpatterns pattern)
    `(list (list :sequence (list :sequence ,(unparse-pattern alg) ,(unparse-pattern key))))))


(defstruct (algorithm-identifier (:include constructor-pattern)
                                 (:constructor make-algorithm-identifier (algorithm parameters
                                                                          &aux (subpatterns (list algorithm parameters))))))

(defmethod constructor-pattern-destructor-sharable-p ((x algorithm-identifier) (y algorithm-identifier))
  t)

(defmethod constructor-pattern-make-destructor ((pattern algorithm-identifier) var)
  (make-destructor :predicate-form `(and (consp ,var)
                                         (cdr (%assoc :sequence ,var))
                                         (vectorp (cdr (%assoc :object-identifier (cdr (%assoc :sequence ,var))))))
                   :accessor-forms (list `(cdr (%assoc :object-identifier (cdr (%assoc :sequence ,var))))
                                         `(second (cdr (%assoc :sequence ,var))))))

(defmethod parse-constructor-pattern ((name (eql 'algorithm-identifier)) &rest args)
  (apply #'make-algorithm-identifier (mapcar #'parse-pattern args)))

(defmethod unparse-pattern ((pattern algorithm-identifier))
  (destructuring-bind (alg params)
      (constructor-pattern-subpatterns pattern)
    `(list (list :sequence
                 (cons :object-identifier ,(unparse-pattern alg))
                 ,(unparse-pattern params)))))


(defstruct (subject-public-key (:include constructor-pattern)
                               (:constructor make-subject-public-key (key
                                                                      &aux (subpatterns (list key))))))

(defmethod constructor-pattern-destructor-sharable-p ((x subject-public-key) (y subject-public-key))
  t)

(defmethod constructor-pattern-make-destructor ((pattern subject-public-key) var)
  (make-destructor :predicate-form `(vectorp (cdr (%assoc :bit-string ,var)))
                   :accessor-forms (list `(cdr (%assoc :bit-string ,var)))))

(defmethod parse-constructor-pattern ((name (eql 'subject-public-key)) &rest args)
  (apply #'make-subject-public-key (mapcar #'parse-pattern args)))

(defmethod unparse-pattern ((pattern subject-public-key))
  `(list (cons :bit-string ,(unparse-pattern (first (constructor-pattern-subpatterns pattern))))))
