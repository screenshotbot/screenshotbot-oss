;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util.test-cdn
  (:use :cl
   :alexandria
        :fiveam))
(in-package :util.test-cdn)

(def-suite* :util.test-cdn)

(test should-be-set-up-by-default
  (is (numberp util.cdn:*cdn-cache-key*)))
