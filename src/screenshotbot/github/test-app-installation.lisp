(defpackage :screenshotbot/github/test-app-installation
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/github/app-installation
                #:%app-installation-id
                #:*app-installation-cache*
                #:app-installation-repos
                #:repos
                #:app-installation-by-id
                #:github-get-access-token-for-installation
                #:update-app-installation
                #:delete-app-installation)
  (:import-from #:screenshotbot/github/plugin
                #:github-plugin)
  (:import-from #:screenshotbot/github/jwt-token
                #:github-api-error
                #:github-request)
  (:import-from #:util/mock-recording
                #:track-during-recording
                #:with-recording)
  (:import-from #:cl-mock
                #:answer
                #:if-called)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/github/test-app-installation)


(util/fiveam:def-suite)

(defparameter *private-key*
  (uiop:read-file-string
   (asdf:system-relative-pathname
    :screenshotbot
    "github/fixture/private-key-traditional.pem"))
  "A fake private key generated just for this test. See
private-key.README.md for how this was generated.")

(def-fixture state ()
  (with-test-store ()
    (let ((*app-installation-cache* (make-hash-table :test #'equal)))
      (cl-mock:with-mocks ()
        (cl-mock:if-called 'github-plugin
                           (lambda ()
                             (make-instance 'github-plugin
                                            :app-id 4242
                                            :private-key *private-key*)))
        (cl-mock:if-called 'github-get-access-token-for-installation
                           (lambda (install-id
                                    &key app-id
                                      private-key)
                             "dummy-token"))
        (&body)))))

(test delete-non-existent-app
  (with-fixture state ()
    (finishes (delete-app-installation 22))))

(test update-app-installation-happy-path
  (with-fixture state ()
    (cl-mock:if-called 'github-request
                        (lambda (url &key installation-token)
                          (let ((page (parse-integer (car (last (str:split "=" url))))))
                            (case page
                              (0
                               `((:repositories
                                  ((:full--name . "screenshotbot/blah"))
                                  ((:full--name . "tdrhq/car")))))
                              (t
                               nil)))))
    (finishes (update-app-installation 23))
    (let ((app-installation (app-installation-by-id 23)))
      (is (equal (list "screenshotbot/blah" "tdrhq/car")
                 (app-installation-repos app-installation))))

    (delete-app-installation 23)
    (is (null (app-installation-by-id 23)))))

(test app-installation-id
  (with-fixture state ()
    (if-called 'github-request
               (lambda (url &key jwt-token)
                 (is (equal "/repos/tdrhq/fast-example/installation" url))
                 `((:id . 222)
                   ;; some other info
                   (:installtion . nil))))
    (is (eql 222
             (%app-installation-id  "tdrhq/fast-example")))))

(test app-installation-id-nil
  (with-fixture state ()
    (if-called 'github-request
               (lambda (url &key jwt-token)
                 (error 'github-api-error
                        :code 404
                        :message "foo")))
    (is (eql nil (%app-installation-id "tdrhq/fast-example")))))
