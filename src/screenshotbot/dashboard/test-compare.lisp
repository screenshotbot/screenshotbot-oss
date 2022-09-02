(defpackage :screenshotbot/dashboard/test-compare
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/compare
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
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/dashboard/test-compare)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
   (tmpdir:with-tmpdir (dir)
     (let ((im1 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image.png"))
           (im2 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-2.png"))
           (im3 (asdf:system-relative-pathname :screenshotbot "dashboard/fixture/image-3.png"))
           (objs))
       (labels ((save (x)
                  (push x objs)
                  x)
                (make-screenshot (img)
                  (let* ((image-blob (save (make-instance 'image-blob)))
                         (image (save (make-instance 'screenshotbot/model:image
                                                      :blob image-blob))))
                    (uiop:copy-file img (blob-pathname image-blob))
                    (save (make-instance 'screenshot
                                          :name "foobar"
                                          :image image)))))
         (unwind-protect
              (&body)
           (loop for x in objs do (delete-object x))))))))

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
