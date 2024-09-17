;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/slack/test-core
  (:use #:cl
        #:fiveam)
  (:import-from #:core/installation/installation
                #:abstract-installation
                #:installation
                #:*installation*)
  (:import-from #:screenshotbot/slack/core
                #:slack-app-redirect-uri))
(in-package :screenshotbot/slack/test-core)


(util/fiveam:def-suite)

(test slack-app-redirect-uri
  (let ((*installation*
          (make-instance 'abstract-installation
                         :domain "https://foo.example.com")))
   (is (equal "https://foo.example.com/slack-app-redirect"
              (slack-app-redirect-uri))))
  (let ((*installation*
          (make-instance 'abstract-installation
                         :domain "https://foo.example.com/")))
    (is (equal "https://foo.example.com/slack-app-redirect"
               (slack-app-redirect-uri)))))

