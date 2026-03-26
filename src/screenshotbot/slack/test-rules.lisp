;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/slack/test-rules
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/slack/rules
                #:tag-rule
                #:matches-rule)
  (:import-from #:auth/viewer-context
                #:viewer-context))
(in-package :screenshotbot/slack/test-rules)


(util/fiveam:def-suite)


(def-fixture state ()
  (with-test-store ()
    (let* ((company (make-instance 'company))
           (channel (make-instance 'channel :company company)))
      (&body))))


(test preconditions
  (with-fixture state ()
    (let ((run (make-recorder-run :channel channel
                                  :screenshots nil)))
     (pass))))

(test try-matching-things
  (with-fixture state ()
    (let ((run (make-recorder-run :channel channel
                                  :company company
                                  :screenshots nil
                                  :tags (list "foo" "bar")))
          (tag-rule (make-instance 'tag-rule
                                   :company company
                                   :tag "bar"
                                   :slack-channel "#general")))
      (is-true (matches-rule tag-rule run)))))

(test failed-matching
  (with-fixture state ()
    (let ((run (make-recorder-run :channel channel
                                  :company company
                                  :screenshots nil
                                  :tags (list "foo" "bar")))
          (tag-rule (make-instance 'tag-rule
                                   :company company
                                   :tag "zoidberg"
                                   :slack-channel "#general")))
      (is-false (matches-rule tag-rule run)))))


