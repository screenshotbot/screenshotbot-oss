;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-figma
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/figma
                #:update-figma-link
                #:figma-link-url
                #:find-existing-figma-link
                #:figma-link)
  (:import-from #:util/store/store
                #:with-test-store))
(in-package :screenshotbot/model/test-figma)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
   (let* ((channel (make-instance 'screenshotbot/model/channel:channel
                                  :name "test-channel"))
          (figma-link (make-instance 'figma-link
                                     :channel channel
                                     :screenshot-name "test-screenshot"
                                     :url "https://www.google.com"
                                     :image (make-instance 'screenshotbot/model/image:image))))
     (&body))))

(test preconditions
  (with-fixture state ()
    (pass)))

(test find-figma-link
  (with-fixture state ()
    (is (equal figma-link
               (find-existing-figma-link
                :channel channel
                :screenshot-name "test-screenshot")))))

(test update-figma-link
  (with-fixture state ()
    (is (equal "https://www.google.com"
               (figma-link-url
                (find-existing-figma-link
                 :channel channel
                 :screenshot-name "test-screenshot"))))
    (update-figma-link :channel channel :screenshot-name "test-screenshot"
                       :url "https://example.com")
    (is (equal "https://example.com"
               (figma-link-url
                (find-existing-figma-link
                 :channel channel
                 :screenshot-name "test-screenshot"))))))
