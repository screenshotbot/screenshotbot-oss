(defpackage :screenshotbot/sdk/test-android
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/android
                #:read-android-metadata
                #:make-image-bundle)
  (:import-from #:screenshotbot/sdk/bundle
                #:list-images)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/sdk/test-android)

(util/fiveam:def-suite)

(test metadata-integration
  (let* ((artifact-dir (asdf:system-relative-pathname
                        :screenshotbot.sdk "example-firebase-artifacts/artifacts/"))
         (metadata (path:catfile artifact-dir "sdcard/screenshots/com.facebook.testing.screenshot.example.test/screenshots-default/metadata.json")))
    (let ((bundle
           (make-image-bundle
            :metadata metadata)))
      (let ((images (list-images bundle)))
        (pass)))))
