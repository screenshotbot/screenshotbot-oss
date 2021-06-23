;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/slack/test-settings
  (:use #:cl
        #:alexandria
        #:bknr.datastore
        #:fiveam
        #:../model
        #:./core
        #:./settings
        #:../login/common))

(def-suite* :screenshotbot/slack/test-settings)

(def-fixture state ()
  (util:with-fake-request ()
   (let* ((company (make-instance 'company))
          (token (make-instance 'slack-token
                                 :access-token "foo"
                                 :company company
                                 :ts 34)))
     (unwind-protect
          (let ((*current-company-override* company))
            (&body))
       (when (default-slack-config company)
         (delete-object (default-slack-config company)))
       (delete-object company)
       (delete-object token)))))

(test posting-when-nothing-is-available ()
  (with-fixture state ()
    (catch 'hunchentoot::handler-done
      (post-settings-slack
       "#general"
       t))
    (is (eql token (access-token (default-slack-config company))))))
