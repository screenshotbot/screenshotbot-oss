(defpackage :screenshotbot/dashboard/test-compare
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/compare
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
                    (let* ((image (make-image :pathname img :for-tests t)))
                      (make-instance 'screenshot
                                      :name "foobar"
                                      :image image))))
           (&body)))))))


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
