;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/dashboard/test-history
    (:use #:cl
          #:alexandria
          #:fiveam
          #:screenshotbot/screenshot-api
          #:screenshotbot/factory)
  (:import-from #:screenshotbot/dashboard/history
                #:render-history)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/screenshot-api
                #:get-screenshot-history)
  (:import-from #:screenshotbot/model/image
                #:image-hash))

(util/fiveam:def-suite)

(defclass my-screenshot (test-screenshot)
  ())

(defmethod screenshot-image ((screenshot my-screenshot))
  (make-instance 'test-image))

(defun make-history-iterator ()
  (let ((screenshots (list (make-instance 'my-screenshot
                                           :name "one")
                           (make-instance 'my-screenshot
                                           :name "one")
                           (make-instance 'my-screenshot
                                           :name "one")))
        (runs (list (make-instance 'test-recorder-run
                                    :commit "one")
                    (make-instance 'test-recorder-run
                                    :commit "two")
                    (make-instance 'test-recorder-run
                                    :commit "three"))))
    (lambda ()
      (values
       (list (pop screenshots)
             (pop runs)
             (car screenshots))
       screenshots))))

(test simple-render-history
  (let ((ctr 0))
   (cl-mock:with-mocks ()
     (let ((remaining-screenshots ))
       (cl-mock:if-called 'get-screenshot-history
                           (lambda (channel screenshot-name &key iterator)
                             (declare (ignore channel screenshot-name))
                             (is-true iterator)
                             (make-history-iterator)))
       (cl-mock:if-called 'image-hash
                           (lambda (image)
                             (incf ctr))))
     (with-fake-request ()
       (auth:with-sessions ()
         (render-history
          :screenshot-name "foo"
          :channel (make-instance 'test-channel)))))) ()
  (pass))
