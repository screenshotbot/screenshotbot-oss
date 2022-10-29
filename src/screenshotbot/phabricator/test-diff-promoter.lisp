;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/phabricator/test-diff-promoter
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/promote-api
                #:plugin-promoter)
  (:import-from #:screenshotbot/phabricator/plugin
                #:phabricator-plugin)
  (:import-from #:screenshotbot/phabricator/diff-promoter
                #:phabricator-promoter)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/phabricator/test-diff-promoter)


(util/fiveam:def-suite)

(test plugin-promoter-happy-path
  (assert-that
   (plugin-promoter (make-instance 'phabricator-plugin))
   (has-typep 'phabricator-promoter)))
