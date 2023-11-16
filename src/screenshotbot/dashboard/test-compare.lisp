(defpackage :screenshotbot/dashboard/test-compare
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/compare
                #:metrics-page
                #:warmup-comparison-images-sync
                #:link-to-run
                #:random-non-alpha-px
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
  (:import-from #:fiveam-matchers
                #:is-string
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:import-from #:screenshotbot/magick/magick-lw
                #:with-pixel-wand
                #:pixel-set-color
                #:magick-new-image
                #:with-wand)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/dashboard/test-compare)


(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*installation* (make-instance 'installation)))
   (with-test-store ()
     (tmpdir:with-tmpdir (dir)
       (with-fake-request ()
         (let ((im1 #.(asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image.png"))
               (im2 #.(asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-2.png"))
               (im3 #.(asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-3.png"))
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
      (finishes (random-zoom-to-on-result imc nil))
      (let ((res
              (json:decode-json-from-string (random-zoom-to-on-result imc nil))))
        (is (eql 360 (a:assoc-value res :width)))
        (is (eql 360 (a:assoc-value res :height)))
        (is (< -1 (a:assoc-value res :x) 360))
        (is (< -1 (a:assoc-value res :y) 360))))))

(test metrics-page-happy-path
  "Since this uses FLI, we need to be extra careful about testing it."
  (with-fixture state ()
    (let ((imc (make-instance 'image-comparison
                              :before (make-image :pathname im1 :for-tests t)
                              :after (make-image :pathname im2 :for-tests t)
                              :result (make-screenshot im1))))
      (finishes (metrics-page imc nil)))))

(test random-non-alpha-for-no-alphas
  (with-fixture state ()
    (with-wand (wand)
      (with-pixel-wand (pw)
        (pixel-set-color pw "none")
        (magick-new-image wand 10 10 pw)
        (multiple-value-bind (x y) (random-non-alpha-px wand nil)
          (is (eql -1 x))
          (is (eql -1 y)))))))

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

(test link-to-empty-run
  (assert-that (format nil "~a" (link-to-run :run nil))
               (contains-string "empty run")))

(test warmup-comparison-images
  (with-fixture state ()
    (let ((run (make-recorder-run
                :screenshots (list (make-screenshot im1))))
          (to (make-recorder-run
               :screenshots (list (make-screenshot im2)))))
      (finishes
       (warmup-comparison-images-sync run to)))))
