(defpackage :util/tests/test-lru-cache
  (:use #:cl
        #:fiveam)
  (:import-from #:util/lru-cache
                #:cache-size
                #:with-cache-file
                #:item-key
                #:cons-map
                #:queue-tail
                #:item-size
                #:file-atime
                #:queue-length
                #:queue-count
                #:queue-head
                #:lru-cache)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:local-nicknames (#:a #:alexandria)
                    (#:~ #:fiveam-matchers)))
(in-package :util/tests/test-lru-cache)

(util/fiveam:def-suite)

(defvar *atimes*)

(def-fixture state ()
  (let ((*atimes* (make-hash-table :test #'equal)))
    (tmpdir:with-tmpdir (dir)
      (&body))))

(defclass test-lru-cache (lru-cache)
  ())

(defmethod file-atime ((cache test-lru-cache) file)
  (let ((val
          (gethash (namestring file) *atimes*)))
    (assert val)
    val))


(test preconditions
  (with-fixture state ()
    (let ((cache (make-instance 'lru-cache
                                :dir dir)))
      (is (equal '()
                 (queue-head cache)))
      (is (equal 0 (queue-count cache)))
      (is (equal 0 (queue-length cache))))))

(defun touch (file)
  (ensure-directories-exist file)
  (with-open-file (stream file :direction :output)
    (declare (ignore stream))))

(defun pathname= (a b)
  (string= (namestring a)
           (namestring b)))

(defun set-atime (file atime)
  (setf (gethash (namestring file) *atimes*) atime))

(test some-files
  (with-fixture state ()
    (let ((file-1 (path:catfile dir "foo.txt")))
      (touch file-1)
      (set-atime file-1 20)
      (let ((cache (make-instance 'test-lru-cache
                                  :dir dir)))
        (~:assert-that
         (queue-head cache)
         (~:has-length 1))
        (let ((item
                (car (queue-head cache))))
          (is (eql 0 (item-size item)))
          (is (equal "foo.txt" (item-key item)))
          (is (equal 1 (hash-table-count
                        (cons-map cache))))
          (is (equal item
                     (car
                      (gethash "foo.txt"
                               (cons-map cache))))))))))

(test sub-dir
  (with-fixture state ()
    (let ((file-1 (path:catfile dir "blah/foo.txt")))
      (touch file-1)
      (set-atime file-1 20)
      (let ((cache (make-instance 'test-lru-cache
                                  :dir dir)))
        (~:assert-that
         (queue-head cache)
         (~:has-length 1))
        (let ((item
                (car (queue-head cache))))
          (is (eql 0 (item-size item)))
          (is (equal "blah/foo.txt" (item-key item)))
          (is (equal 1 (hash-table-count
                        (cons-map cache))))
          (is (equal item
                     (car
                      (gethash "blah/foo.txt"
                               (cons-map cache))))))))))

(defun write-test-string (file)
  (with-open-file (stream file :direction :output)
    (write-string "arnold" stream)))

(test file-sizes
  (with-fixture state ()
    (let ((file-1 (path:catfile dir "foo.txt")))
      (write-test-string file-1)
      (set-atime file-1 20)
      (let ((cache (make-instance 'test-lru-cache
                                  :dir dir)))
        (~:assert-that
         (queue-head cache)
         (~:has-length 1))
        (let ((item
                (car (queue-head cache))))
          (is (eql 6 (item-size item)))
          (is (eql 6 (cache-size cache)))
          (is (equal "foo.txt" (item-key item))))))))

(test ordering
  (with-fixture state ()
    (let ((keys (loop for i from 0 to 20
                       for key = (format nil "file-~d.txt"  (+ (* 100 (random 100)) i))
                       for file = (path:catfile
                                   dir key)
                       do
                          (progn
                            (touch file)
                            (set-atime file i))
                       collect
                      key)))
      (let ((cache (make-instance 'test-lru-cache
                                  :dir dir)))
        (assert-that
         (loop for item in (queue-head cache)
               collect (item-key item))
         (apply #'contains keys))
        (assert-that
         (namestring (item-key (car (queue-tail cache))))
         (is-equal-to (car (last keys))))
        (is (equal 21 (queue-length cache)))
        (is (equal 21 (queue-count cache)))))))

(def-fixture defaults ()
  (let ((cache (make-instance 'test-lru-cache
                              :dir dir)))
    (&body)))

(test with-cache-file
  (with-fixture state ()
    (with-fixture defaults ()
      (with-cache-file (pathname cache "foo.txt")
        (write-test-string pathname))
      (is (equal 1 (length (queue-head cache))))
      (is (equal 1 (queue-length cache)))
      (is (equal 1 (queue-count cache)))
      (is (equal 6 (cache-size cache)))
      (with-cache-file (pathname cache "foo.txt")
        nil)
      (is (equal 2 (length (queue-head cache))))
      (is (equal 2 (queue-length cache)))
      (is (equal 1 (queue-count cache)))
      (is (equal 6 (cache-size cache)))
      (with-cache-file (pathname cache "foo.txt")
        nil)
      (is (equal 3 (length (queue-head cache))))
      (is (equal 3 (queue-length cache)))
      (is (equal 1 (queue-count cache)))
      (is (equal 1 (length (remove-if #'null (queue-head cache))))))))


(test trim-queue
  (with-fixture state ()
    (with-fixture defaults ()
      (dotimes (i 1000)
        (with-cache-file (pathname cache "foo.txt")
          (ignore-errors
           (touch pathname))))
      (is (>= 20 (length (queue-head cache))))
      (is (>= 20 (queue-length cache)))
      (is (= 1 (queue-count cache))))))
