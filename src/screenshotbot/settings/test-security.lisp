(defpackage :screenshotbot/settings/test-security
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:util/testing
                #:screenshot-static-page)
  (:import-from #:screenshotbot/settings/security
                #:settings-security-page)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/settings/test-security)


(util/fiveam:def-suite)

(def-fixture state (&key oauth)
  (with-test-store ()
    (with-test-user (:user user :logged-in-p t)
      (unless oauth
        (with-transaction ()
         (setf (auth:user-password user) "foobar")))
      (&body))))

(test simple-screenshot
  (with-fixture state ()
   (screenshot-static-page
    :screenshotbot
    "security-settings-page"
    (settings-security-page))))

(test simple-screenshot-for-success
  (with-fixture state ()
   (screenshot-static-page
    :screenshotbot
    "security-settings-page-for-success-changed"
    (settings-security-page :success t))))

(test simple-screenshot-for-oauth
  (with-fixture state (:oauth t)
   (screenshot-static-page
    :screenshotbot
    "security-settings-page-for-oauth"
    (settings-security-page))))
