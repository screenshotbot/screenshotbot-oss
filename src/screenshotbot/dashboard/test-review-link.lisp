;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-review-link
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/review-link
                #:bad-url-page
                #:validate-url)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:util/testing
                #:with-fake-request
                #:screenshot-static-page)
  (:import-from #:auth
                #:with-sessions)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/server
                #:screenshotbot-template)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/dashboard/test-review-link)


(util/fiveam:def-suite)

(test validate-url
  (is (equal "https://www.google.com" (validate-url "https://www.google.com")))
  (assert-that (validate-url "git@gdfdf:foo/bar")
               (has-typep 'nibble:nibble)))

(test bad-url-page
  (with-installation ()
   (with-fake-request ()
     (with-sessions ()
       (screenshot-static-page
        :screenshotbot
        "bad-url-page"
        (let ((core/ui/template:*app-template*
                (make-instance 'screenshotbot-template)))
         (bad-url-page "git@dfd:foo/bar")))))))
