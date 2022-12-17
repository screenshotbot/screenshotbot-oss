(defpackage :screenshotbot/api/test-core
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/api/core
                #:with-api-key
                #:defapi)
  (:import-from #:cl-mock
                #:answer
                #:with-mocks)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/api/test-core)


(util/fiveam:def-suite)

(defapi (%dummy-1 :uri "/api/dummy") ()
  "OK")

(test returns
  (is (equal "OK" (%dummy-1))))

(defapi (%dummy-2 :uri "/api/dummy-2") (name)
  (format nil "OK ~a" name))

(test simple-param
  (is (equal "OK zoidberg"
             (%dummy-2 :name "zoidberg"))))

(defapi (%dummy-with-int :uri "/api/dummy-2") ((name :parameter-type 'integer))
  (format nil "OK ~d" name))

(test dummy-with-int
  (is (equal "OK 3"
             (%dummy-with-int :name 3))))

(define-condition my-error (error)
  ())

(test with-api-key-for-parameters
  (with-mocks ()
    (answer (hunchentoot:authorization) nil)
    (answer (hunchentoot:parameter "api-key")  "foo")
    (answer (hunchentoot:parameter "api-secret-key") "bar")
    (with-api-key (key secret)
      (is (equal "foo" key))
      (is (equal "bar" secret)))))

(test with-api-key-for-authorization
  (with-mocks ()
    (answer (hunchentoot:authorization)
      (values "foo" "bar"))
    (with-api-key (key secret)
      (is (equal "foo" key))
      (is (equal "bar" secret)))))
