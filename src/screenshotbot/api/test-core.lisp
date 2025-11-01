(defpackage :screenshotbot/api/test-core
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/api/core
                #:*wrap-internal-errors*
                #:authenticate-request-from-key
                #:authenticate-api-request
                #:result
                #:api-error
                #:error-result-message
                #:error-result-stacktrace
                #:with-error-handling
                #:with-api-key
                #:defapi)
  (:import-from #:cl-mock
                #:if-called
                #:answer
                #:with-mocks)
  (:import-from #:fiveam-matchers/core
                #:assert-that
                #:equal-to)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:import-from #:util/json-mop
                #:json-mop-to-string)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:with-test-user)
  (:import-from #:core/api/model/api-key
                #:encode-api-token
                #:api-key
                #:api-key-secret-key)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:auth/model/roles
                #:user-role)
  (:import-from #:screenshotbot/installation
                #:multi-org-feature
                #:installation)
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

(test internal-error-gets-logged
  (with-mocks ()
    (with-fake-request ()
     (let ((calledp nil))
       (if-called 'sentry-client:capture-exception
                  (lambda (e)
                    (setf calledp t)))
       (let ((message
               (with-error-handling ()
                 (error 'my-error))))

         (assert-that (error-result-stacktrace message)
                      (contains-string "Stacktrace ID" ))
         (assert-that (error-result-message message)
                      (contains-string "Internal error"))
         (assert-that calledp
                      (described-as
                          "capture-exception should've been called"
                        (equal-to t))))))))

(define-condition my-simple-error-1 (error)
  ())

(test flag-to-wrap-internal-errors
  (let ((*wrap-internal-errors* nil))
    (signals my-simple-error-1
      (with-error-handling ()
        (error 'my-simple-error-1)))))


(test api-error-is-propagated-but-not-logged
  (with-mocks ()
    (with-fake-request ()
     (let ((calledp nil))
       (if-called 'sentry-client:capture-exception
                  (lambda (e)
                    (setf calledp t)))
       (let ((message
               (with-error-handling ()
                 (error 'api-error :message "bleh bleh"))))

         (assert-that (error-result-stacktrace message)
                      (contains-string "Stacktrace ID" ))
         (assert-that (error-result-message message)
                      (equal-to "bleh bleh"))
         (assert-that calledp
                      (described-as
                          "capture-exception should not be called"
                        (equal-to nil))))))))


(test api-result-can-be-encoded
  (assert-that (json-mop-to-string (make-instance 'result :success t))
               (contains-string "success")))

(defclass my-installation (multi-org-feature
                           installation)
  ())

(def-fixture state ()
  (with-installation (:installation (make-instance 'my-installation))
   (with-test-store ()
     (with-test-user (:user user :company company)
       (let ((api-key (make-instance 'api-key
                                     :user user
                                     :company company)))
         (&body))))))

(test authenticate-api-request
  (with-fixture state ()
    (with-fake-request ()
      (finishes
       (authenticate-request-from-key hunchentoot:*request*
                                      api-key)))))

(test if-you-create-an-api-key-for-a-company-you-no-longer-can-view
  (with-fixture state ()
    (with-fake-request ()
      (setf (user-role company user) nil)
      ;; The API key might still be used by CI jobs! See T1138
      (finishes
        (authenticate-request-from-key hunchentoot:*request*
                                       api-key)))))

(test authenticate-api-request-extracts-key-from-secret
  "Test that API key is automatically extracted from API secret when not provided"
  (with-fixture state ()
    (with-fake-request ()
      (with-mocks ()
        ;; Provide empty api-key but full encoded token as api-secret
        (let ((encoded-token (encode-api-token api-key)))
          (answer (hunchentoot:authorization) (values "" encoded-token))
          (finishes
            (authenticate-api-request hunchentoot:*request*)))))))

(test authenticate-api-request-fails-with-mismatched-key
  "Test that authentication fails when encoded secret has mismatched API key"
  (with-fixture state ()
    (with-fake-request ()
      (with-mocks ()
        ;; Provide a different api-key than what's in the encoded token
        (let ((encoded-token (encode-api-token api-key)))
          (answer (hunchentoot:authorization) (values "wrong-key" encoded-token))
          (signals api-error
            (authenticate-api-request hunchentoot:*request*)))))))

(test authenticate-api-request-fails-without-key-and-plain-secret
  "Test that authentication fails when using plain secret without API key"
  (with-fixture state ()
    (with-fake-request ()
      (with-mocks ()
        ;; Provide empty api-key and plain (non-encoded) secret
        ;; This should fail because we can't extract key from plain secret
        (let ((plain-secret (api-key-secret-key api-key)))
          (answer (hunchentoot:authorization) (values "" plain-secret))
          (signals api-error
            (authenticate-api-request hunchentoot:*request*)))))))
