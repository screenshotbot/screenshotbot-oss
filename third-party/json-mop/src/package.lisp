;; Copyright (c) 2015 Grim Schjetne
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(defpackage #:json-mop
  (:use #:cl)
  (:import-from #:yason
                #:true
                #:false
                #:parse
                #:encode
                #:encode-array-element
                #:encode-object-element
                #:with-output
                #:with-array
                #:with-object)
  (:import-from #:anaphora
                #:awhen
                #:it)
  (:export #:json-serializable
           #:json-serializable-class
           #:to-lisp-value
           #:to-json-value
           #:json-to-clos
           ;; Re-export yason:encode
           #:encode
           ;; Conditions
           #:slot-not-serializable
           #:slot-name
           #:json-type-error
           #:json-type
           #:null-value
           #:null-in-homogenous-sequence
           #:no-values-parsed
           #:no-values-hash-table
           #:no-values-class))
