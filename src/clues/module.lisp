(defpackage :clues/module
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:defmodule
   #:provider-get
   #:make-module
   #:get-provider
   #:binder))
(in-package :clues/module)

(defvar *injector-modules* nil
  "A list of all known modules, used for computing dependencies
  interactively when one module changes.")

(defgeneric provider-get (x))

(defclass binder ()
  ((providers :initform (make-hash-table)
              :reader providers)))

(defclass module ()
  ())

(defmethod initialize-instance :after ((module module) &key &allow-other-keys)
)

(defgeneric build-bindings (module binder))

(defmethod bind-provider ((binder binder) key &key to-provider)
  (setf (gethash key (providers binder))
        to-provider))

(defmacro defmodule (name &body rules)
  `(progn
     (defclass ,name (module)
       ())

     (defmethod build-bindings ((module ,name) (binder binder))
       ,@ (loop for rule in rules
                collect
                (ecase (car rule)
                  (:bind
                      `(bind-provider
                         binder
                         ,@ (cdr rule)))
                  (:depends-on
                   nil))))))

(defmethod make-module (name)
  (make-instance name))

(defmethod get-provider ((binder binder) name)
  (gethash name (providers binder)))
