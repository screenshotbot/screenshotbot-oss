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
  (:import-from #:screenshotbot/phabricator/builds
                #:*build-info-index*
                #:build-info)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:promoter-pull-id)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/phabricator/test-diff-promoter)


(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*build-info-index* (make-hash-table :test #'equal)))
   (with-test-store ()
     (let ((promoter (make-instance 'phabricator-promoter))
           (company (make-instance 'company)))
       (&body)))))

(test plugin-promoter-happy-path
  (with-fixture state ()
   (assert-that
    (plugin-promoter (make-instance 'phabricator-plugin))
    (has-typep 'phabricator-promoter))))

(test promoter-pull-id-for-phabricator
  (with-fixture state ()
    (make-instance 'build-info
                   :company company
                   :diff 20
                   :revision 5)
    (let ((run (make-recorder-run
                :company company
                :phabricator-diff-id "20")))
      (is (eql 5 (promoter-pull-id promoter run))))))
