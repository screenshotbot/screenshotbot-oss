;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/github/test-github-app
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/github/github-app
                #:last-cache-ts
                #:github-app-installation-ids
                #:transient-github-app)
  (:import-from #:fset
                #:@))
(in-package :screenshotbot/github/test-github-app)


(util/fiveam:def-suite)


(test installation-id-cache
  (let ((app (make-instance 'transient-github-app)))
    (is (eql 0 (fset:size (github-app-installation-ids app))))
    (setf (@ (github-app-installation-ids app) "tdrhq/slite") "1234") 
    (is (eql 1 (fset:size (github-app-installation-ids app))))
    (is (> (last-cache-ts app) 0))
    (setf (last-cache-ts app) (- (get-universal-time) 600))
    (is (eql 0 (fset:size (github-app-installation-ids app))))))
