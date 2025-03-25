(defpackage :auth/test-avatar
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:bknr.datastore
                #:blob-pathname
                #:persistent-class
                #:store-object)
  (:import-from #:auth/avatar
                #:overriden-avatar
                #:handle-avatar)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:hunchentoot
                #:*reply*)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/login/oidc
                #:oidc-user)
  (:import-from #:cl-mock
                #:with-mocks))
(in-package :auth/test-avatar)

(util/fiveam:def-suite)

(defclass fake-user (store-object)
  ((oauth-users :accessor auth:oauth-users
                :initform nil)
   (email :accessor auth:user-email
          :initform nil))
  (:metaclass persistent-class))

(def-fixture state ()
  (with-mocks ()
   (with-test-store ()
     (with-fake-request ()
       (let ((user (make-instance 'fake-user)))
         (&body))))))

(def-easy-macro assert-redirects (url &fn fn)
  (catch 'hunchentoot::handler-done
    (fn))
  (is (equal (hunchentoot:header-out :location *reply*)
             url)))

(test gravatar ()
  (with-fixture state ()
    (assert-redirects ("https://secure.gravatar.com/avatar/852438d026c018c4307b916406f98c62")
      (handle-avatar user))))

(test with-avatar-from-oidc
  (with-fixture state ()
    (setf (auth:oauth-users user)
          (list (make-instance 'oidc-user
                               :avatar "https://example.com/foo.png")))
    (assert-redirects ("https://example.com/foo.png")
      (handle-avatar user))))

(test overriden-avatar
  (with-fixture state ()
    (let ((override (make-instance 'overriden-avatar
                                   :user user
                                   :content-type "image/png")))
      (with-open-file (stream (blob-pathname override) :direction :output)
        (write-string "foobar" stream))

      (cl-mock:if-called 'hunchentoot:handle-static-file
                         (lambda (filename content-type)
                           (is (equal (blob-pathname override) filename))
                           (is (equal content-type "image/png"))
                           'dummy))
      (is (eql 'dummy (handle-avatar user))))))
