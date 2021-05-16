(defpackage :azula/js
  (:use :cl
   :alexandria)
  (:import-from :azula/main
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
