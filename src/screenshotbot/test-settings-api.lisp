(defpackage :screenshotbot/test-settings-api
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/settings-api
                #:all-settings
                #:defsettings
                #:*settings*)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:screenshotbot/server
                #:staging-p)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-settings-api)


(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
    (let ((*settings* nil)
          (*installation* (make-instance
                           'installation)))
      (&body))))

(test filter-staging
  (with-fixture state ()
    (cl-mock:if-called 'staging-p
                       (lambda ()
                         nil))
    (defsettings one
      :name "one"
      :handler 'one)
    (assert-that (all-settings *installation*)
                 (has-length 1))
    (defsettings two
      :name "two"
      :staging-p t
      :handler 'two)
    (assert-that
     (all-settings *installation*)
     (has-length 1))))

(test do-not-filter-staging-on-actual-staging
  (with-fixture state ()
    (cl-mock:if-called 'staging-p
                       (lambda ()
                         t))
    (defsettings one
      :name "one"
      :handler 'one)
    (assert-that (all-settings *installation*)
                 (has-length 1))
    (defsettings two
      :name "two"
      :staging-p t
      :handler 'two)
    (assert-that
     (all-settings *installation*)
     (has-length 2))))
