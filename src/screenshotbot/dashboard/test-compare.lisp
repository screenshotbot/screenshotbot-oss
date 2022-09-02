(defpackage :screenshotbot/dashboard/test-compare
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/compare
                #:random-zoom-to-on-result
                #:image-comparison
                #:do-image-comparison)
  (:import-from #:screenshotbot/model/image
                #:image
                #:image-blob
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
                #:with-fake-request)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/dashboard/test-compare)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
   (tmpdir:with-tmpdir (dir)
     (with-fake-request ()
       (let ((im1 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image.png"))
             (im2 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-2.png"))
             (im3 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-3.png"))
             (objs))
         (labels ((make-screenshot (img)
                    (let* ((image-blob (make-instance 'image-blob))
                           (image (make-instance 'screenshotbot/model:image
                                                  :blob image-blob)))
                      (uiop:copy-file img (blob-pathname image-blob))
                      (make-instance 'screenshot
                                      :name "foobar"
                                      :image image))))
           (&body)))))))

(test do-image-comparison
  (with-fixture state ()
    (is-true (uiop:file-exists-p im1))
    (is-true (uiop:file-exists-p im2))
    (uiop:with-temporary-file (:pathname out :type "png")
      (is-false
       (do-image-comparison (make-screenshot im1) (make-screenshot im3) out nil)))
    (uiop:with-temporary-file (:pathname out :type "png")
      (is-true
       (do-image-comparison (make-screenshot im1) (make-screenshot im2) out nil)))))

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
