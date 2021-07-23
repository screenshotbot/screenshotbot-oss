;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/test-assets
  (:use #:cl
        #:alexandria
        #:fiveam)
  (:import-from #:./assets
                #:prepare-delivered-asset-map))

(util/fiveam:def-suite)

;; this is an expensive test!
(test delivered-asset-map
  (let ((res (prepare-delivered-asset-map :screenshotbot :copy nil)))
    (is (equal (list "assets/screenshotbot.js-assets/screenshotbot.js-assets.js"
                     "assets/screenshotbot.js-assets/screenshotbot.js-assets.js.map")
               (assoc-value res :screenshotbot.js-assets)))
    (is (equal nil (assoc-value res :screenshotbot)))))
