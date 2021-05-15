(defpackage :azula/test-main
  (:use :cl
   :alexandria
        :fiveam)
  (:import-from :azula/main
                :make-hash-from-string))
(in-package :azula/test-main)

(test make-hash
  (is (equalp #(11 238 199 181 234 63 15 219 201 93 13 212 127 60 91 194 117 218 138 51)
              (make-hash-from-string "foo"))))
