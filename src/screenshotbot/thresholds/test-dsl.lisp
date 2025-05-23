;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/thresholds/test-dsl
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/thresholds/dsl
                #:eval-dsl
                #:%read-dsl-from-string
                #:%read-dsl)
  (:local-nicknames
   (#:dsl #:screenshotbot/thresholds/dsl)))
(in-package :screenshotbot/thresholds/test-dsl)

(util/fiveam:def-suite)

(test read-exact
  (is
   (equal
    '(dsl:exact-images)
    (%read-dsl-from-string "(exact-images)"))))

(test read-with-whitespace
  (is
   (equal
    '(dsl:exact-images)
    (%read-dsl-from-string "( exact-images )")))
  (is
   (equal
    '(dsl:exact-images)
    (%read-dsl-from-string "  ( exact-images )  "))))

(test nested
  (is
   (equal
    '((dsl:exact-images (dsl:exact-images)))
    (%read-dsl-from-string "(( exact-images (exact-images) ))"))))

(test numbers
  (is
   (equal
    '(22)
    (%read-dsl-from-string "(22)"))))

(test evalate
  (is
   (equal 3
          (funcall
           (eval-dsl
            (%read-dsl-from-string "(+ 1 2)"))))))

