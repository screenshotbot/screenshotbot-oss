(pkg:define-package :azula/js
  (:use :cl
   :alexandria)
  (:import-from ./main
   :target
   :cache-key)
  (:export :js-library
           :js-binary))
(in-package :azula/js)

(defclass js-library (target)
  ())

(defclass js-binary (target)
  ())

(defmethod cache-key (executor (target js-library))
  (call-next-method))

#+nil
(defun glob (expr &key (directory (build-file-pathname *current-build-file*)))
  (let ((parts (str:split "/" expr :limit 2)))
    (cond
      ((equal "parts ")))))
