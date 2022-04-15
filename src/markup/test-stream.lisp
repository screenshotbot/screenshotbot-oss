(defpackage :markup.test-stream
  (:use :cl
        :fiveam
        :markup)
  (:import-from #:markup/stream
                #:wrap-stream
                #:read-so-far)
  (:export))
(in-package :markup.test-stream)

(def-suite* :markup.test-stream)

(test simple-flow
  (let ((stream (make-string-input-stream "foobar car war")))
    (let ((copy (wrap-stream stream)))
      (is (equal "" (read-so-far copy)))
      (read-char copy)
      (read-char copy)
      (read-char copy)

      (is (equal "foo" (read-so-far copy)))
      (read-char copy)

      (is (equal "foob" (read-so-far copy)))
      (is (equal #\a (peek-char nil copy)))
      (is (equal "foob" (read-so-far copy))))))
