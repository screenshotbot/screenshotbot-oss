(defpackage :util/tests/test-request
  (:use #:cl
        #:fiveam)
  (:import-from #:util/request
                #:make-header-hash-table)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-request)

(util/fiveam:def-suite)

(test make-header-hash-table
  (let ((headers (list
                  (cons "Foo" "bar"))))
    (let ((res (make-header-hash-table headers)))
      (is (hash-table-p res))
      (is (equal "bar" (gethash "foo" res))))))
