(defpackage :screenshotbot/github/test-app-installation
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/github/app-installation
                #:app-installation-repos
                #:repos
                #:app-installation-by-id
                #:github-get-access-token-for-installation
                #:update-app-installation
                #:delete-app-installation)
  (:import-from #:screenshotbot/github/plugin
                #:github-plugin)
  (:import-from #:screenshotbot/github/jwt-token
                #:github-request)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/github/test-app-installation)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (cl-mock:with-mocks ()
      (cl-mock:if-called 'github-plugin
                          (lambda ()
                            (make-instance 'github-plugin
                                            :app-id 4242
                                            :private-key "foobar")))
      (cl-mock:if-called 'github-get-access-token-for-installation
                          (lambda (install-id
                                   &key app-id
                                     private-key)
                            "dummy-token"))
      (&body))))

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
