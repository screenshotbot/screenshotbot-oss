;; Copyright (c) 2016 Grim Schjetne
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

(in-package #:json-mop-tests)

(def-suite encode-decode
  :in test-all
  :description "Test encoding and decoding slots")

(in-suite encode-decode)

(test string
  (for-all ((obj (gen-object)))
    (is (equal (get-string obj)
               (get-string (obj-rt obj))))))

(test number
  (for-all ((obj (gen-object)))
    (is (= (get-number obj)
           (get-number (obj-rt obj))))))

;; TODO: test hash table

(test vector
  (for-all ((obj (gen-object)))
    (is (equalp (get-vector obj)
                (get-vector (obj-rt obj))))))

(test list
  (for-all ((obj (gen-object)))
    (is (equal (get-list obj)
               (get-list (obj-rt obj))))))

(test bool
  (for-all ((obj (gen-object)))
    (is (eql (get-bool obj)
             (get-bool (obj-rt obj))))))

(test object
  (for-all ((obj (gen-object)))
    (is (= (get-number (get-object obj))
           (get-number (get-object (obj-rt obj)))))))

(test inheritance
  (let ((child (make-instance 'child))
        (parent-only (make-instance 'parent)))
    (is (string= (with-output-to-string (s) (encode child s))
                 (with-output-to-string (s) (encode parent-only s))))))
