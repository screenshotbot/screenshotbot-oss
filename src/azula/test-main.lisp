(pkg:define-package :azula/test-main
    (:use #:cl
          #:alexandria
          #:fiveam)
  (:import-from ./main
                #:make-hash-from-string
                #:config
                #:*targets*
                #:*config*
                #:cache-key
                #:executor)
  (:import-from ./scanner
                #:scan))

(test make-hash
  (is (equalp #(11 238 199 181 234 63 15 219 201 93 13 212 127 60 91 194 117 218 138 51)
              (make-hash-from-string "foo"))))

(defun write-azula (dir exprs)
  (with-open-file (stream (path:catfile dir "AZULA") :direction :output)
    (loop for expr in exprs do
      (write expr :stream stream))))

(test cache-key ()
  (tmpdir:with-tmpdir (dir)
   (let ((*targets* nil)
         (*config* (make-instance 'config
                                  :root (namestring dir))))
     (write-azula dir `((js-library :name "foo" :srcs (list "bar.js"))))
     (with-open-file (stream (path:catfile dir "bar.js") :direction :output))
     (scan)
     (is (equal 1 (length *targets*)))
     (let ((executor (make-instance 'executor
                                    :config *config*)))
       (is (equalp #(190 27 222 192 170 116 180 220 176 121 148 62 112 82 128 150 204 169 133 248) (cache-key executor (car *targets*))))

       (is (vectorp (cache-key executor (car *targets*))))))))
