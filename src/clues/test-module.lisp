(defpackage :clues/test-module
  (:use #:cl
        #:fiveam
        #:clues/module)
  (:import-from #:clues/injector
                #:get-instance)
  (:import-from #:clues/module
                #:build-bindings)
  (:local-nicknames (#:a #:alexandria)))
(in-package :clues/test-module)


(util/fiveam:def-suite)

(defvar *inst* 'foo)

(defclass inst-provider ()
  ())

(defmethod provider-get ((self inst-provider))
  *inst*)

(defmodule dummy-module
  (:bind 'str :to-provider (make-instance 'inst-provider)))

(test create-module
  (is-true (make-module 'dummy-module))
  (let ((module (make-module 'dummy-module))
        (binder (make-instance 'binder)))
    (build-bindings module binder)
    (let ((provider (get-provider binder 'str)))
      (is (eql 'foo (provider-get provider))))))
