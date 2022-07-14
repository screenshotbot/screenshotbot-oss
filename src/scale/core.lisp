(defpackage :scale/core
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:create-instance
   #:delete-instance))
(in-package :scale/core)

(defgeneric create-instance (provider type &key region))

(defgeneric delete-instance (provider instance))
