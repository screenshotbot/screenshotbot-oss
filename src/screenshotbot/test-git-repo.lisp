;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-git-repo
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/git-repo
                #:generic-git-repo
                #:null-repo
                #:public-repo-p))
(in-package :screenshotbot/test-git-repo)


(util/fiveam:def-suite)


(def-fixture state ()
  (&body))

(test public-repo-p
  (is-false (public-repo-p (make-instance 'generic-git-repo)))
  (is-false (public-repo-p (make-instance 'null-repo))))
