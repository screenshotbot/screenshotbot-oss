;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-promote-api
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/promote-api
                #:plugin-promoter
                #:register-promoter
                #:list-promoters
                #:*promoters*)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:screenshotbot/installation
                #:*installation*
                #:installation)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-promote-api)

(util/fiveam:def-suite)

(def-fixture state (&key plugins)
  (let ((*installation* (make-instance 'installation
                                       :plugins plugins)))
    (let ((*promoters* nil))
      (&body))))

(test empty-list
  (with-fixture state ()
    (is (eql nil (list-promoters)))))

(defclass foo ()
  ())

(test list-promoters
  (with-fixture state ()
    (register-promoter 'foo)
    (assert-that (list-promoters)
                 (contains (has-typep 'foo)))))

(defclass simple-plugin ()
  ())

(defmethod plugin-promoter ((plugin simple-plugin))
  "promoter")

(test list-promoters-with-plugins
  (with-fixture state (:plugins (list (make-instance 'simple-plugin)))
    (assert-that (list-promoters)
                 (contains "promoter"))))
