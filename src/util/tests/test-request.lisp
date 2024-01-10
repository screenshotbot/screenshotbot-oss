(defpackage :util/tests/test-request
  (:use #:cl
        #:fiveam)
  (:import-from #:util/request
                #:wrap-ssl-errors
                #:fix-bad-chars
                #:http-request
                #:make-header-hash-table)
  (:import-from #:hunchentoot
                #:easy-acceptor
                #:define-easy-handler)
  (:import-from #:util/testing
                #:with-local-acceptor)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-request)

(util/fiveam:def-suite)

(test make-header-hash-table
  (let ((headers (list
                  (cons "Foo" "bar"))))
    (let ((res (make-header-hash-table headers)))
      (is (hash-table-p res))
      (is (equal "bar" (gethash "foo" res))))))

(defvar *body* nil)
(defvar *gzip* nil)

(define-easy-handler (dummy-request :uri "/test-dummy-content"
                                    :acceptor-names '(dummy)) ()
  (cond
    (*gzip*
     (setf (hunchentoot:header-out :content-encoding)
           "gzip")
     *body*)
    (t
     *body*)))

(def-fixture state ()
  (unwind-protect
       (with-local-acceptor (url) ('test-acceptor)
         (&body))
    (setf *body* nil)
    (setf *gzip* nil)))

(defclass test-acceptor (easy-acceptor)
  ()
  (:default-initargs :name 'dummy))

(defun read-file-seq (file)
  (with-open-file (stream
                   (path:catfile
                    #.(asdf:system-source-directory :util)
                    file)
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((output (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (read-sequence output stream)
      output)))

(test decoding-gzip
  (with-fixture state ()
    (setf *body* (read-file-seq "test-file.txt"))
    (multiple-value-bind (content ret headers)
        (http-request
         (format nil "~a~a" url "/test-dummy-content")
         :force-binary t)
      (is (equalp content
                  (read-file-seq "test-file.txt")))
      (is
       (equal "12" (a:assoc-value headers :content-length))))

    (setf *gzip* t)
    (setf *body* (read-file-seq "test-file-compressed.txt.gz"))

    (multiple-value-bind (content ret headers)
        (http-request
         (format nil "~a~a" url "/test-dummy-content")
         :accept-gzip t
         :decode-content t
         :force-binary t)
      (is (equalp content
                  (read-file-seq "test-file.txt")))
      (is (equal "gzip"
                 (a:assoc-value headers :content-encoding)))
      (is
       (equal "32" (a:assoc-value headers :content-length))))))

(test if-content-encoding-is-not-set-we-still-decode
  (with-fixture state ()
    (setf *gzip* t)
    (setf *body* (read-file-seq "test-file-compressed.txt.gz"))
    (multiple-value-bind (content ret headers)
        (http-request
         (format nil "~a~a" url "/test-dummy-content")
         :decode-content t
         :force-binary t)
      (is (equalp content
                  (read-file-seq "test-file.txt")))
      (is (equal "gzip"
                 (a:assoc-value headers :content-encoding)))
      (is
       (equal "32" (a:assoc-value headers :content-length))))))

(test if-decode-content-is-nil-we-dont-decode
  (with-fixture state ()
    (setf *gzip* t)
    (setf *body* (read-file-seq "test-file-compressed.txt.gz"))
    (multiple-value-bind (content ret headers)
        (http-request
         (format nil "~a~a" url "/test-dummy-content")
         :decode-content nil
         :force-binary t)
      (is (equalp content
                  (read-file-seq "test-file-compressed.txt.gz")))
      (is (equal "gzip"
                 (a:assoc-value headers :content-encoding)))
      (is
       (equal "32" (a:assoc-value headers :content-length))))))

(test fix-bad-chars
  (is (equal "https://www.google.com?foo=bar"
             (fix-bad-chars "https://www.google.com?foo=bar")))
    (is (equal "https://www.google.com?foo=bar%7Ccar"
             (fix-bad-chars "https://www.google.com?foo=bar|car"))))

#+lispworks
(test wrap-ssl-errors
  (multiple-value-bind (body ret)
      (wrap-ssl-errors ()
        (error 'comm:ssl-closed))
    (is (eql 502 ret)))
  (signals comm:ssl-closed
    (wrap-ssl-errors (:ensure-success t)
      (error 'comm:ssl-closed))))
