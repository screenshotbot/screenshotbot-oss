(pkg:define-package :deadbeef
    (:use #:cl
          #:alexandria)
  (:use-reexport :deadbeef/impl))
