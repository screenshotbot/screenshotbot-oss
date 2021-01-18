(in-package :cl-user)

(defpackage :bknr.skip-list
  (:use :cl)
  (:export
   #:skip-list
   #:skip-list-length
   #:skip-list-insert
   #:skip-list-get
   #:skip-list-delete
   #:skip-list-remove
   #:skip-list-search

   #:sl-cursor-next
   #:sl-cursor-prev
   #:skip-list-cursor
   #:skip-list-value-cursor
   #:skip-list-key-cursor
   #:skip-list-values-cursor
   #:skip-list-keys-cursor
   #:skip-list-range-cursor
   #:map-skip-list
   #:map-skip-list-values))
