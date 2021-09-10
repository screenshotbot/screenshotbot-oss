(uiop:define-package :screenshotbot/github/test-plugin
  (:use #:cl
        #:fiveam
        #:alexandria)
  (:import-from #:screenshotbot/github/plugin
                #:make-github-repo))
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
