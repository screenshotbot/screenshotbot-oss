(defpackage :azula/js
  (:use :cl
   :alexandria)
  (:import-from :azula/main
   :target)
  (:export :js-library
           :js-binary))
(in-package :azula/js)

(defclass js-library (target)
  ())

(defclass js-binary (target)
  ())
