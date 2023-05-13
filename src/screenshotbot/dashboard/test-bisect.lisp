;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-bisect
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/screenshot-api
                #:make-screenshot)
  (:import-from #:screenshotbot/testing
                #:fix-timestamps
                #:snap-all-images
                #:with-installation
                #:screenshot-test)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/dashboard/bisect
                #:render-bisection
                #:bisect-item
                #:state)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:easy-macros
                #:def-easy-macro))
(in-package :screenshotbot/dashboard/test-bisect)

(util/fiveam:def-suite)

(defun %make-image (x)
  (make-image
   :pathname (path:catfile
              #.(asdf:system-relative-pathname
                 :screenshotbot "fixture/")
              x)))

(def-easy-macro wrap-snapshot (&fn fn)
  (snap-all-images)
  (fix-timestamps (fn)))

(def-fixture test-state ()
  (with-installation ()
    (with-test-store ()
      (with-fake-request ()
        (auth:with-sessions ()
         (let* ((im-1 (%make-image "wizard.png"))
                (screenshot-1 (make-screenshot :image im-1
                                               :name "foo"))
                (channel (make-instance 'channel))
                (run-1 (make-recorder-run
                        :channel channel
                        :screenshots (list screenshot-1)))
                (run-2 (make-recorder-run
                        :channel channel
                        :screenshots (list screenshot-1))))
           (&body)))))))

(screenshot-test bisect-with-user-interaction-required
  (with-fixture test-state ()
    (wrap-snapshot ()
     (render-bisection
      (make-instance 'state
                     :items (loop for i from 0 to 5
                                  collect
                                  (make-instance 'bisect-item
                                                 :screenshot screenshot-1
                                                 :run run-1)))))))

(screenshot-test bisect-end
  (with-fixture test-state ()
    (wrap-snapshot ()
     (render-bisection
      (make-instance 'state
                     :items (loop for i from 0 to 1
                                  collect
                                  (make-instance 'bisect-item
                                                 :screenshot screenshot-1
                                                 :run run-1)))))))

(screenshot-test bisect-with-wide-image
  (with-fixture test-state ()
    (let* ((wide-im (%make-image "wide.png"))
           (screenshot (make-screenshot :image wide-im
                                        :name "foo"))
           (run (make-recorder-run
                 :channel channel
                 :screenshots (list screenshot))))
      (wrap-snapshot ()
        (render-bisection
         (make-instance 'state
                        :items (loop for i from 0 to 5
                                     collect
                                     (make-instance 'bisect-item
                                                    :screenshot screenshot
                                                    :run run))))))))

(screenshot-test bisect-with-large-square
  (with-fixture test-state ()
    (let* ((wide-im (%make-image "large-square.png"))
           (screenshot (make-screenshot :image wide-im
                                        :name "foo"))
           (run (make-recorder-run
                 :channel channel
                 :screenshots (list screenshot))))
      (wrap-snapshot ()
        (render-bisection
         (make-instance 'state
                        :items (loop for i from 0 to 5
                                     collect
                                     (make-instance 'bisect-item
                                                    :screenshot screenshot
                                                    :run run))))))))
