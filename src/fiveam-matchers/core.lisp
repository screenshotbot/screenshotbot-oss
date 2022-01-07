(uiop:define-package :fiveam-matchers/core
    (:use #:cl
          #:alexandria)
  (:export
   #:equal-to
   #:is-not
   #:matcher
   #:assert-that
   #:describe-self
   #:describe-mismatch
   #:single-value-matcher
   #:self-describing-list
   #:has-all
   #:has-any
   #:has-typep
   #:ensure-matcher
   #:is-string
   #:matchesp))
(in-package :fiveam-matchers/core)

(defclass matcher ()
  ())


(defun render-description (description output)
  (loop for x in description
        do
           (etypecase x
             (string
              (format output "~a" description))
             (list
              (render-description x output)))))

(defgeneric matchesp (matcher value))

(defgeneric describe-mismatch (matcher value))

(defmethod describe-self ((matcher matcher))
  `(" matches a matcher of type " ,(type-of matcher)))

;; core matchers

(defclass single-value-matcher (matcher)
  ((value :initarg :value
          :reader value)))

(defclass equal-to (single-value-matcher)
  ())

(defun equal-to (val)
  (make-instance 'equal-to :value val))

(defun ensure-matcher (val)
  (typecase val
    (matcher val)
    (t (equal-to val))))

(defmethod matchesp ((matcher equal-to) value)
  (equal (value matcher) value))

(defmethod describe-mismatch ((matcher equal-to) value)
  `("was " ,(esc value)))

(defmethod describe-mismatch (matcher value)
  `("No description of mismatch available, see expression "))

(defmethod describe-self ((matcher equal-to))
  (esc (value matcher)))

(defclass is-not (matcher)
  ((value :initarg :value
          :reader value)))

(defun is-not (value)
  (check-type value matcher)
  (make-instance 'is-not :value value))

(defmethod matchesp ((matcher matcher) value)
  ;; This allows for specializing of matchesp on the value method
  nil)

(defmethod matchesp ((is-not is-not) value)
  (not (matchesp (value is-not) value)))

(defmethod describe-self ((is-not is-not))
  `("not " ,(describe-self (value is-not))))

(defmethod describe-mismatch ((is-not is-not) value)
  `("not " ,(describe-mismatch (value is-not) value)))

;; finally, an assert-that function

(defclass escaped ()
  ((value :initarg :value
          :reader value)))

(defun esc (x)
  (make-instance 'escaped :value x))

(defclass self-describing-list ()
  ((value :initarg :value
          :reader value)
   (start :initarg :start
          :initform "["
          :reader start)
   (end :initarg :end
        :initform "]"
        :reader end)
   (sep :initarg :sep
        :reader sep
        :initform ", ")))

(defun self-describing-list (list &rest args)
  (apply 'make-instance 'self-describing-list
                 :value list
                 args))

(defun format-description (description stream)
  (etypecase description
    (list
     (loop for x in description do
       (format-description x stream)))
    (escaped
     (format stream "~S" (value description)))
    (self-describing-list
     (format stream "~a" (start description))
     (loop for (x . next) on (value description)
           do
              (progn
                (format-description (describe-self x)
                                    stream)
                (when next
                  (format stream "~a" (sep description)))))
     (format stream "~a" (end description)))
    (t
     (format stream "~a" description))))

(defclass has-all (matcher)
  ((matchers :initarg :matchers
             :reader matchers)))

(defclass has-any (matcher)
  ((matchers :initarg :matchers
             :reader matchers)))

(defmethod matchesp ((has-all has-all) value)
  (not
   (loop for matcher in (matchers has-all)
         unless (matchesp matcher value)
           return t)))

(defmethod matchesp ((has-any has-any) value)
  (loop for matcher in (matchers has-any)
        if (matchesp matcher value)
          return t))

(defun check-matcher-list (matchers)
  (dolist (x matchers)
    (check-type x matcher)))

(defun has-all (&rest matchers)
  (check-matcher-list matchers)
  (make-instance 'has-all :matchers matchers))

(defun has-any (&rest matchers)
  (check-matcher-list matchers)
  (make-instance 'has-any :matchers matchers))

(defclass has-typep (single-value-matcher)
  ())

(defun has-typep (type)
  (make-instance 'has-typep :value type))

(defun is-string ()
  (has-typep 'string))

(defmethod matchesp ((matcher has-typep) value)
  (typep value (value matcher)))

(defmethod describe-self ((matcher has-typep))
  `("has type " ,(value matcher)))

(Defmethod describe-mismatch ((matcher has-typep) value)
  `("has type " ,(type-of value)))

(defmethod describe-mismatch-to-string (matcher value)
  (format
   nil
   "Expected: ~a
     but: ~a"
   (with-output-to-string (s)
     (format-description (describe-self matcher) s))
   (with-output-to-string (s)
     (format-description (describe-mismatch matcher value) s))))

(defun call-assert-that (value matcher expression match-expression)
  (cond
    ((matchesp matcher value)
     (fiveam::add-result 'fiveam::test-passed
                         :test-expr `(assert-that
                                      ,expression
                                      ,match-expression)))
    (t
     (fiveam::process-failure
      `(assert-that
        ,expression
        ,match-expression)
      "~a"
      (describe-mismatch-to-string
       matcher value)))))

(defmacro assert-that (value &rest matchers)
  (alexandria:with-gensyms (value-sym)
   `(let ((,value-sym ,value))
      ,@(loop for matcher in matchers collect
            `(call-assert-that
              ,value-sym
              ,matcher
              ',value
              ',matcher)))))
