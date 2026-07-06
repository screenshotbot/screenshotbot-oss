(uiop:define-package :screenshotbot/github/test-plugin
  (:use #:cl
        #:fiveam
        #:alexandria)
  (:import-from #:screenshotbot/github/plugin
                #:%app-name
                #:fetch-app-name
                #:app-name
                #:github-plugin
                #:make-github-repo)
  (:import-from #:screenshotbot/github/github-app
                #:github-app-private-key
                #:github-app-id
                #:github-app))
(in-package :screenshotbot/github/test-plugin)


(util/fiveam:def-suite)

(test make-github-repo-cache
  (let ((company-1 :fake-company)
        (company-2 :fake-company-2)
        (link "https://github.com/foo/bar.git"))
    (is (eql (make-github-repo :link link :company company-1)
             (make-github-repo :link link :company company-1)))
    (is (not
         (eql (make-github-repo :link link :company company-1)
              (make-github-repo :link link :company company-2))))))

(defclass fake-github-plugin (github-plugin)
  ())

(defmethod fetch-app-name ((self fake-github-plugin))
  "zoidberg")

(test app-name-is-fetched-when-needed
  (let ((plugin (make-instance 'fake-github-plugin
                                      :app-id 12232
                                      :private-key "dfdfd")))
    (is (eql nil (%app-name plugin)))
    (is (equal "zoidberg"
               (app-name plugin)))
    (is (equal "zoidberg" (%app-name plugin)))))

(test github-app-object-is-cached
  (let ((plugin (make-instance 'github-plugin
                               :app-id "foo"
                               :private-key "PEM")))
    (let ((app (github-app plugin :company-1)))
      (is-true app)
      (is (eql app (github-app plugin :company-1)))
      (is (equal "foo"
                 (github-app-id app)))
      (is (equal "PEM"
                 (github-app-private-key app))))))
