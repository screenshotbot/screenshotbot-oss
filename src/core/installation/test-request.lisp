;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(defpackage :core/installation/test-request
  (:use #:cl
        #:fiveam)
  (:import-from #:core/installation/installation
                #:*secondary-installations*
                #:*installation*)
  (:import-from #:core/installation/request
                #:installation-matches-request-p
                #:with-installation-for-request)
  (:import-from #:fiveam-matchers/core
                #:assert-that
                #:is-equal-to))
(in-package :core/installation/test-request)


(util/fiveam:def-suite)

(test happy-path-for-request
  (let ((*installation* 'foo))
    (assert-that
     (with-installation-for-request (:fake-request)
       :after)
     (is-equal-to :after))))

(test happy-path-when-there-are-secondary-requests
  (let ((*installation* 'foo)
        (*secondary-installations* (list 'bar 'nar)))
    (assert-that
     (with-installation-for-request (:fake-request)
       *installation*)
     (is-equal-to 'foo))))

(defmethod installation-matches-request-p ((installation (eql 'boo)) request)
  t)


(test secondary-installation-wins
  (let ((*installation* 'foo)
        (*secondary-installations* (list 'bar 'boo 'nar)))
    (assert-that
     (with-installation-for-request (:fake-request)
       *installation*)
     (is-equal-to 'boo))))
