;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/installation/test-installation
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:core/installation/installation
                #:installation-domain
                #:abstract-installation
                #:installation
                #:*installation*)
  (:import-from #:core/config/model
                #:config))
(in-package :core/installation/test-installation)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((*installation* (make-instance 'abstract-installation
                                         :domain "https://example.com/")))
      (&body))))

(test installation-domain-without-any-store
  (with-fixture state ()
    (is (equal "https://example.com/" (installation-domain *installation*)))))

(test we-prefer-the-config-value
  (with-fixture state ()
    (is (equal "https://example.com/" (installation-domain *installation*)))
    (setf (config "installation.domain") "https://foobar.com")
    (is (equal "https://foobar.com" (installation-domain *installation*)))))

(test signals-warning-when-not-matching
  (with-fixture state ()
    (is (equal "https://example.com/" (installation-domain *installation*)))
    (setf (config "installation.domain") "https://foobar.com")
    (signals simple-warning
      (installation-domain *installation*))))
