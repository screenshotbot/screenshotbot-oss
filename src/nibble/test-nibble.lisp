(defpackage :nibble/test-nibble
  (:use #:cl
        #:fiveam)
  (:import-from #:nibble
                #:expired-nibble
                #:make-id
                #:nibble-url
                #:nibble-current-user
                #:defnibble
                #:nibble
                #:render-nibble)
  (:local-nicknames (#:a #:alexandria)))
(in-package :nibble/test-nibble)

(def-suite* :nibble)
(def-suite* :nibble/test-nibble :in :nibble)

(defclass fake-acceptor (nibble:nibble-acceptor-mixin)
  ())

(defmethod nibble-current-user ((acceptor fake-acceptor))
  :dummy-user)

(def-fixture state ()
  (let ((hunchentoot:*acceptor* (make-instance 'fake-acceptor)))
   (&body)))

(test preconditions
  (with-fixture state ()
   (is (equal "foobar"
              (render-nibble
               hunchentoot:*acceptor*
               (nibble ()
                 "foobar"))))))


(defnibble foo ()
  "foobar2")

(test named-nibble
  (with-fixture state ()
    (is (equal "foobar2"
               (render-nibble
                hunchentoot:*acceptor*
                (nibble foo))))))

(defnibble foo-with-args (name)
  (format nil "hello ~a" name))

(test named-nibble-with-args
  (with-fixture state ()
    (util/testing:with-fake-request (:script-name "/?name=arnold")
      (auth:with-sessions ()
       (is (equal "hello arnold"
                  (render-nibble
                   hunchentoot:*acceptor*
                   (nibble foo-with-args))))))))

(test with-name-renders-url
  (with-fixture state ()
    (util/testing:with-fake-request ()
      (auth:with-sessions ()
       (let ((nibble (nibble (:name :foobar)
                       "")))
         (is (str:ends-with-p "?_n=foobar"
                              (nibble-url  nibble))))))))

(test render-nibble
  (with-fixture state ()
    (is (str:containsp "NIBBLE"
                       (format nil "~a" (nibble () "dummy"))))))

(test make-id-happy-path
  (with-fixture state ()
    (is (> (make-id (get-universal-time)) 10000000))))

(test expired-nibble
  (with-fixture state ()
    (util/testing:with-fake-request ()
      (render-nibble hunchentoot:*acceptor* "342343243343432232213123123")
      (is (eql 410 (hunchentoot:return-code hunchentoot:*reply*)))
      (is (equal "1" (hunchentoot:header-out :x-expired-nibble))))))

(test expired-nibble-signals-warnings
  (with-fixture state ()
    (util/testing:with-fake-request ()
      (signals expired-nibble
       (render-nibble hunchentoot:*acceptor* "342343243343432232213123123")))))
