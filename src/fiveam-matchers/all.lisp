(uiop:define-package :fiveam-matchers
    (:use #:cl)
  (:use-reexport #:fiveam-matchers/core
                 #:fiveam-matchers/lists
                 #:fiveam-matchers/has-length
                 #:fiveam-matchers/every-item
                 #:fiveam-matchers/strings))
