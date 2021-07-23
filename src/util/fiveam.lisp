(pkg:define-package :util/fiveam
    (:use #:cl
          #:alexandria)
  (:export #:def-suite))

;;(setf fiveam::*toplevel-suites* nil)
;; (setf fiveam::*tests* nil)

(defun %def (parts)
  (let ((keywords (symbol-package :foo)))
   (cond
     ((null (cdr parts))
      (let ((name (intern (car parts) keywords)))
       (or
        (fiveam::get-test name)
        (setf (fiveam::get-test name)
              (fiveam:make-suite name)))))
     (t
      (let ((parent (%def (butlast parts))))
        (assert parent)
        (let* ((name (intern (str:join "/" parts) keywords)))
          (or
           (let ((suite (fiveam::get-test name)))
             (when suite
               (check-type suite fiveam::test-suite)
               suite))
           (let ((ret
                   (fiveam:make-suite name
                                      :in
                                      (fiveam::name parent))))
             ;; hold on one sec. Let's be very sure that the parent has the child
             (assert (gethash name (fiveam::tests parent)))
             ret))))))))


(defun def-suite-recursive (name)
  (let ((parts (str:split "/" (string name))))
    (%def parts)))

(defmacro def-suite ()
  `(eval-when (:load-toplevel :execute)
     (let ((suite-name ,(guess-suite-name)))
       (def-suite-recursive suite-name)
       (eval `(fiveam::%in-suite ,suite-name)))))

(defun guess-suite-name ()
  (format t "Guessing suite name for ~a~%" *package*)
  (intern (string (package-name *package*))
          (symbol-package :foo)))
