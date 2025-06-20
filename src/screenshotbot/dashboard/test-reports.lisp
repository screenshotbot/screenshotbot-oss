;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-reports
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/reports
                #:%render-sorted-by-changes
                #:report-page
                #:render-acceptable-history
                #:submit-share-report)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:snap-all-images
                #:fix-timestamps
                #:screenshot-test
                #:with-test-user)
  (:import-from #:screenshotbot/report-api
                #:report)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:fiveam-matchers/core
                #:does-not
                #:equal-to
                #:has-typep
                #:assert-that)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/model/sharing
                #:share)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:screenshotbot/model/report
                #:acceptable-history-item
                #:base-acceptable)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:import-from #:util/store/object-id
                #:oid)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/model/company
                #:redirect-url
                #:company)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/screenshot-api
                #:make-screenshot)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/dashboard/compare
                #:warmup-comparison-images-sync
                #:warmup-report)
  (:import-from #:screenshotbot/model/testing
                #:with-test-image)
  (:import-from #:util/testing
                #:with-fake-request)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/dashboard/test-reports)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store (:globally t)
    (with-test-user (:company company :user user
                     :logged-in-p t)
      (let ((report (make-instance 'report)))
       (&body)))))

(test submit-share-report
  (with-fixture state ()
    (catch 'hunchentoot::handler-done
      (submit-share-report report nil))

    (let ((shares (class-instances 'share)))
      (assert-that shares
                   (has-length 1)))))

(screenshot-test render-acceptable-history-empty ()
  (with-fixture state ()
    (let ((report (make-instance 'report))
          (acceptable (make-instance 'base-acceptable
                                     :report report)))
      (render-acceptable-history acceptable))))

(screenshot-test render-acceptable-history-non-empty ()
  (with-fixture state ()
    (let ((report (make-instance 'report))
          (acceptable (make-instance 'base-acceptable
                                     :report report
                                     :history (list
                                               (make-instance 'acceptable-history-item
                                                              :state :accepted
                                                              :user user)
                                               (make-instance 'acceptable-history-item
                                                              :state :rejected
                                                              :user user)))))
      (fix-timestamps
       (render-acceptable-history acceptable)))))

(def-fixture report-page (&key (logged-in-p t))
  (with-test-store ()
    (with-test-user (:user user :company company :logged-in-p logged-in-p)
     (let* ((channel (make-instance 'channel :company company :name "bleh"))
            (run1 (make-recorder-run :company company :channel channel))
            (run2 (make-recorder-run :company company :channel channel))
            (report (make-instance 'report
                                   :title "1 changes"
                                   :run run1
                                   :previous-run run2)))
       (&body)))))

(test report-page-happy-path ()
  (with-fixture report-page ()
    (assert-that
     (markup:write-html
      (report-page :id (oid report)))
     (contains-string "1 changes"))))


(def-easy-macro get-redirect (&fn fn)
  (catch 'hunchentoot::handler-done
    (funcall fn))
  (hunchentoot:header-out :location))

(test report-page-redirects ()
  (with-fixture report-page ()
    (assert-that
     (get-redirect ()
       (report-page :id (oid report)))
     (equal-to nil))
    (setf (redirect-url company) "https://foo.example.com")
    (assert-that
     (get-redirect ()
       (report-page :id (oid report)))
     (Contains-string "https://foo.example.com/"))))


(screenshot-test image-processing-is-not-complete-yet
  (with-fixture report-page ()
    (let* ((im1 (make-image :pathname
                            (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image.png")))
           (im2 (make-image :pathname
                            (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-3.png")))
           (run1 (make-recorder-run :channel channel
                                    :screenshots (list (make-screenshot
                                                        :name "foo"
                                                        :image im1))))
           (run2 (make-recorder-run :channel channel
                                    :screenshots (list
                                                  (make-screenshot
                                                   :name "foo"
                                                   :image im2))))
           (report (make-instance 'report
                                  :run run2
                                  :previous-run run1)))
      (let ((res (%render-sorted-by-changes report)))
        (assert-that
         (markup:write-html res)
         (contains-string "not complete"))
        res))))

(screenshot-test sorted-by-changes
  (with-fixture report-page ()
    (let* ((im1 (make-image :pathname
                            (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image.png")))
           (im2 (make-image :pathname
                            (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-3.png")))
           (run1 (make-recorder-run :channel channel
                                    :screenshots (list (make-screenshot
                                                        :name "foo"
                                                        :image im1))))
           (run2 (make-recorder-run :channel channel
                                    :screenshots (list
                                                  (make-screenshot
                                                   :name "foo"
                                                   :image im2))))
           (report (make-instance 'report
                                  :run run2
                                  :previous-run run1)))
      (warmup-comparison-images-sync run2 run1)
      (snap-all-images)
      (let ((res (%render-sorted-by-changes report)))
        (assert-that
         (markup:write-html res)
         (does-not (contains-string "not complete")))
        res))))

(screenshot-test report-page-happy-path-2
  (with-fixture report-page ()
    (with-fake-request ()
      (with-test-user (:company company
                       :logged-in-p t)
       (with-test-image (im1-path :pixels '((1 1) (2 2)))
         (with-test-image (im2-path :pixels '((3 3) (4 4)) :color "blue")
           (let* ((im1 (make-image :pathname im1-path))
                  (im2 (make-image :pathname im2-path))
                  (run1 (make-recorder-run :channel channel
                                           :company company
                                           :screenshots (list (make-screenshot
                                                               :name "FakeName"
                                                               :image im1))))
                  (run2 (make-recorder-run :channel channel
                                           :company company
                                           :screenshots (list
                                                         (make-screenshot
                                                          :name "FakeName"
                                                          :image im2))))
                  (report (make-instance 'report
                                         :run run2
                                         :previous-run run1)))
             (warmup-comparison-images-sync run2 run1)
             (snap-all-images)
             (let ((res (report-page :id (oid report))))
               (assert-that
                (markup:write-html res)
                (contains-string "FakeName"))
               res))))))))
