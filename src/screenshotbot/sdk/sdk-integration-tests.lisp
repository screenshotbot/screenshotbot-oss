(defpackage :screenshotbot/sdk/sdk-integration-tests
  (:use #:cl))
(in-package :screenshotbot/sdk/sdk-integration-tests)

(uiop:setup-temporary-directory)

(ql:quickload :screenshotbot.sdk/sdk-integration-tests-impl)

(screenshotbot/sdk/sdk-integration-tests-impl::run-tests-with-tmpdir)

