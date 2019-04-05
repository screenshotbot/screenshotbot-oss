(defpackage #:asn1/format/rsa
  (:use #:cl
        #:asn1/format/public-key)
  (:import-from #:asn1/decode
                #:decode)
  (:import-from #:optima
                #:defpattern)
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
  (:export #:rsa-public-key
           #:rsa-public-key-info
           #:rsa-private-key))
(in-package #:asn1/format/rsa)

(defstruct (rsa-public-key (:include constructor-pattern)
                           (:constructor make-rsa-public-key (modulus public-exponent
                                                              &aux (subpatterns
                                                                    (list modulus public-exponent))))))

(defmethod constructor-pattern-destructor-sharable-p ((x rsa-public-key) (y rsa-public-key))
  t)

(defmethod constructor-pattern-make-destructor ((pattern rsa-public-key) var)
  (let ((it (gensym)))
    (make-destructor :bindings `((,it (cdr (%assoc :sequence (decode ,var)))))
                     :predicate-form it
                     :accessor-forms (list `(cdr (first ,it))
                                           `(cdr (second ,it))))))

(defmethod parse-constructor-pattern ((name (eql 'rsa-public-key)) &rest args)
  (apply #'make-rsa-public-key (mapcar #'parse-pattern args)))

(defmethod unparse-pattern ((pattern rsa-public-key))
  (destructuring-bind (modulus public-exponent)
      (constructor-pattern-subpatterns pattern)
    ;; FIXME: have to encode into ASN.1
    `(list (list :sequence
                 (cons :integer ,(unparse-pattern modulus))
                 (cons :integer ,(unparse-pattern public-exponent))))))

(defstruct (rsa-public-key-info (:include constructor-pattern)
                                (:constructor make-rsa-public-key-info (modulus public-exponent
                                                                        &aux (subpatterns
                                                                              (list modulus public-exponent))))))

(defmethod constructor-pattern-destructor-sharable-p ((x rsa-public-key-info) (y rsa-public-key-info))
  t)

(defmethod constructor-pattern-make-destructor ((pattern rsa-public-key-info) var)
  (let ((it (gensym))
        (key (gensym))
        (m (gensym))
        (e (gensym)))
    (make-destructor :bindings `((,it (optima:match ,var
                                        ((subject-public-key-info
                                          (algorithm-identifier (equalp #(1 2 840 113549 1 1 1)) (equal '(:null)))
                                          (subject-public-key ,key))
                                         (optima:match ,key
                                           ((rsa-public-key ,m ,e)
                                            (list ,m ,e)))))))
                     :predicate-form it
                     :accessor-forms (list `(first ,it)
                                           `(second ,it)))))

(defmethod parse-constructor-pattern ((name (eql 'rsa-public-key-info)) &rest args)
  (apply #'make-rsa-public-key-info (mapcar #'parse-pattern args)))

(defmethod unparse-pattern ((pattern rsa-public-key-info))
  (destructuring-bind (modulus public-exponent)
      (constructor-pattern-subpatterns pattern)
    `(subject-public-key-info
      (algorithm-identifier (equalp #(1 2 840 113549 1 1 1)) (equal '(:null)))
      (subject-public-key (rsa-public-key ,modulus ,public-exponent)))))

(optima:defpattern rsa-private-key (&key version modulus public-exponent private-exponent prime1 prime2 exponent1 exponent2 coefficient other-prime-infos)
  `(list (cons :sequence
               (list* (cons :integer ,(or version '(satisfies integerp)))
                      (cons :integer ,(or modulus '(satisfies integerp)))
                      (cons :integer ,(or public-exponent '(satisfies integerp)))
                      (cons :integer ,(or private-exponent '(satisfies integerp)))
                      (cons :integer ,(or prime1 '(satisfies integerp)))
                      (cons :integer ,(or prime2 '(satisfies integerp)))
                      (cons :integer ,(or exponent1 '(satisfies integerp)))
                      (cons :integer ,(or exponent2 '(satisfies integerp)))
                      (cons :integer ,(or coefficient '(satisfies integerp)))
                      (or null (list ,other-prime-infos))))))
