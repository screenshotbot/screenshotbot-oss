(defpackage :screenshotbot/dashboard/test-compare
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/compare
                #:image-comparison-job
                #:prepare-image-comparison
                #:random-zoom-to-on-result
                #:image-comparison)
  (:import-from #:screenshotbot/model/image
                #:make-image
                #:image
                #:local-image)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:util/testing
                #:screenshot-static-page
                #:with-fake-request)
  (:import-from #:screenshotbot/report-api
                #:report)
  (:import-from #:screenshotbot/testing
                #:snap-all-images
                #:snap-image-blob
                #:with-test-user)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/dashboard/reports
                #:render-report-page)
  (:import-from #:screenshotbot/model/channel
                #:channel)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*)
  (:import-from #:screenshotbot/screenshot-api
                #:make-screenshot)
  (:import-from #:fiveam-matchers/core
                #:is-string
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/dashboard/test-compare)

(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*installation* (make-instance 'installation)))
   (with-test-store ()
     (tmpdir:with-tmpdir (dir)
       (with-fake-request ()
         (let ((im1 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image.png"))
               (im2 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-2.png"))
               (im3 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-3.png"))
               (objs))
           (flet ((make-screenshot (img)
                    (let* ((image (make-image :pathname img :for-tests t)))
                      (make-screenshot
                       :name "foobar"
                       :image image))))
             (&body))))))))


(test random-zoom-to-on-result
  (with-fixture state ()
    (let ((imc (make-instance 'image-comparison
                               :result (make-screenshot im1))))
      (finishes (random-zoom-to-on-result imc))
      (let ((res
              (json:decode-json-from-string (random-zoom-to-on-result imc))))
        (is (eql 360 (a:assoc-value res :width)))
        (is (eql 360 (a:assoc-value res :height)))
        (is (< -1 (a:assoc-value res :x) 360))
        (is (< -1 (a:assoc-value res :y) 360))))))

(test report-screenshot-test
  (with-fixture state ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      (let* ((channel (make-instance 'channel
                                     :company company
                                     :name "bleh"
                                     :github-repo "git@github.com:a/b.gitq"))
             (one (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots (list (make-screenshot im1))))
             (two (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots (list (make-screenshot im2))))
             (report (make-instance 'report
                                    :title "foobar"
                                    :run one
                                    :previous-run two)))
        (snap-all-images)
        (screenshot-static-page
         :screenshotbot
         "report-page"
         (render-report-page report :skip-access-checks t))))))


(test report-with-only-added-screenshots
  (with-fixture state ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      (let* ((channel (make-instance 'channel
                                     :company company
                                     :name "bleh"
                                     :github-repo "git@github.com:a/b.gitq"))
             (one (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots nil))
             (two (make-recorder-run
                   :channel channel
                   :company company
                   :screenshots (list (make-screenshot im2))))
             (report (make-instance 'report
                                    :title "foobar"
                                    :run one
                                    :previous-run two)))
        (snap-all-images)
        (screenshot-static-page
         :screenshotbot
         "report-page-only-added-screenshots"
         (render-report-page report :skip-access-checks t))))))

(test prepare-image-comparison
  (with-fixture state ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      (let ((job (make-instance 'image-comparison-job
                                :before-image (make-screenshot im1)
                                :after-image (make-screenshot im2))))

        (let ((image-comparison (prepare-image-comparison job)))
          (assert-that image-comparison
                       (described-as "Expected to get a json response"
                           (is-string))))))))
