;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/dashboard/test-history
    (:use #:cl
          #:alexandria
          #:fiveam
          #:../screenshot-api
          #:../factory)
  (:import-from #:./history
                #:render-history))

(def-suite* :screenshotbot/dashboard/test-history)

(defclass my-screenshot (test-screenshot)
  ())

(defmethod screenshot-image ((screenshot my-screenshot))
  (make-instance 'test-image))

(test simple-render-history
  (render-history
   :screenshots (list (make-instance 'my-screenshot
                                      :name "one")
                      (make-instance 'my-screenshot
                                      :name "one")
                      (make-instance 'my-screenshot
                                      :name "one"))
   :channel (make-instance 'test-channel)
   :runs (list (make-instance 'test-recorder-run
                               :commit "one")
               (make-instance 'test-recorder-run
                               :commit "two")
               (make-instance 'test-recorder-run
                               :commit "three")))
  (pass))
