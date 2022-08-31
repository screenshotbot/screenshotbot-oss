(pkg:define-package :util/java/test-binding
    (:use #:cl
          #:fiveam
          #:fiveam-matchers)
  (:import-from #:util/java
                #:java-syntax
                #:new-instance)
  (:import-from #:util/java/binding
                #:javafy-name
                #:bind-instance))

(util/fiveam:def-suite)

(named-readtables:in-readtable java-syntax)

(defclass user ()
  ((url :initarg :url
        :accessor user-url)
   (login :initarg :login
          :accessor login)))

(defclass dummy () ())

(def-fixture state ()
  (let ((user (new-instance #,org.eclipse.egit.github.core.User)))
    (#_setUrl user "https://github.com/foo")
    (#_setLogin user "tdrhq")
    (&body)))

(test javafy-name
  (assert-that
   (javafy-name 'foo-bar)
   (equal-to "getFooBar"))
  (assert-that
   (javafy-name 'foo-bar-p)
   (equal-to "isFooBar")))

(test simple-binding
  (with-fixture state ()
    (let ((user (bind-instance 'user user)))
      (assert-that
       user
       (has-typep 'user))
      (assert-that
       (user-url user)
       (equal-to "https://github.com/foo"))
      (assert-that
       (login user)
       (equal-to "tdrhq")))))
