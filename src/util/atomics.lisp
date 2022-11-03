(defpackage :util/atomics
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:atomic-exchange))
(in-package :util/atomics)

(defmacro atomic-exchange (place new-val)
  #+lispworks
  `(system:atomic-exchange ,place ,new-val)
  #-lispworks
  `(let (prev)
     ;; This is not technically the same as atomic-exchange, but it's
     ;; close enough for our needs.
     (atomics:atomic-update ,place
                            (lambda (old)
                              (setf prev old)
                              ,new-val))
     prev))
