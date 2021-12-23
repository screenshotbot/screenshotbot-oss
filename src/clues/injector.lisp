(defpackage :clues/injector
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:injector
   #:get-instance
   #:scope
   #:singleton))
(in-package :clues/injector)

(defclass scope ()
  ((cache :initform (make-hash-table)
          :reader scope-cache)))

(defclass singleton (scope)
  ())

(defclass injector ()
  ((root-module :initarg :root-module
                :reader root-module)
   (scope :initform (list (make-instance 'singleton))
          :reader scope)))

(defmethod get-instance ((injector injector)
                         (class standard-class))
  (make-instance class))

(defmethod get-instance ((injector injector)
                         (sym symbol))
  (get-instance injector (find-class sym)))

(defclass abstract-module ()
  ())

(defmethod get-instance :around (injector arg)
  (restart-case
      (call-next-method)
    (:retry-get-instance ()
      (get-instance injector arg))))
