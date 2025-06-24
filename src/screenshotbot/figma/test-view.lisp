;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/figma/test-view
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:screenshot-test)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/figma/view
                #:associate-figma
                #:perform-update-image-link
                #:download-figma-image)
  (:import-from #:screenshotbot/model/figma
                #:figma-link-image
                #:figma-link-screenshot-name
                #:figma-link-channel
                #:figma-link-url
                #:find-existing-figma-link)
  (:import-from #:util/store/store
                #:with-test-store))
(in-package :screenshotbot/figma/test-view)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
   (with-installation ()
     (with-fake-request ()
       (auth:with-sessions ()
         (&body))))))

(screenshot-test associate-figma-form
  (with-fixture state ()
    (associate-figma)))

(test perform-update-image-link-happy-path
  (with-fixture state ()
    (let ((channel (make-instance 'screenshotbot/model/channel:channel
                                  :name "test-channel"))
          (screenshot-name "test-screenshot")
          (figma-url "https://www.figma.com/file/ABC123/Design-Name?node-id=1%3A2")
          (image-url "https://cdn.figma.com/test-image.png")
          (mock-image-data (make-array 100 :element-type '(unsigned-byte 8) :initial-element 42)))
      
      (cl-mock:with-mocks ()
        (cl-mock:answer (download-figma-image image-url)
          mock-image-data)
        
        (let ((result (catch 'hunchentoot::handler-done
                        (perform-update-image-link
                         :channel channel
                         :screenshot-name screenshot-name
                         :figma-url figma-url
                         :image-url image-url))))
          
          ;; Verify that a figma-link was created
          (let ((figma-link (find-existing-figma-link
                             :channel channel
                             :screenshot-name screenshot-name)))
            (is (not (null figma-link)))
            (is (equal figma-url (figma-link-url figma-link)))
            (is (equal channel (figma-link-channel figma-link)))
            (is (equal screenshot-name (figma-link-screenshot-name figma-link)))
            (is (not (null (figma-link-image figma-link))))))))))






