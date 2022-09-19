;; -*- coding: utf-8 -*-
(defpackage :screenshotbot/sdk/test-firebase
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/firebase
                #:parse-firebase-output
                #:firebase-output-bucket)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/sdk/test-firebase)

(util/fiveam:def-suite)

(defvar *output* "google.com/support/

Uploading [./build/outputs/apk/debug/project-debug.apk] to Firebase Test Lab...
Uploading [./build/outputs/apk/androidTest/debug/project-debug-androidTest.apk] to Firebase Test Lab...
Raw results will be stored in your GCS bucket at [https://console.developers.google.com/storage/browser/cloud-test-screenshotbot-example/2022-09-19_18:03:36.359222_WSoF/]

Test [matrix-fz9c7urm9xoya] has been created in the Google Cloud.
Firebase Test Lab will execute your instrumentation test on 1 device(s).

Test results will be streamed to [https://console.firebase.google.com/project/*********************/testlab/histories/bh.73ae3a33c4730f39/matrices/7660637650631969176].
18:03:44 Test is Pending
18:04:09 Starting attempt 1.
18:04:09 Test is Running
18:05:46 Started logcat recording.
18:05:46 Started crash monitoring.
18:05:46 Preparing device.
18:06:23 Logging in to Google account on device.
18:06:23 Installing apps.
18:06:35 Retrieving Performance Environment information from the device.
18:06:35 Setting up Android test.
18:06:35 Started crash detection.
18:06:35 Started Out of memory detection
18:06:35 Started performance monitoring.
18:06:48 Starting Android test.
18:07:06 Completed Android test.
18:07:06 Stopped performance monitoring.
18:07:06 Tearing down Android test.
18:07:48 Logging out of Google account on device.
18:07:48 Stopped crash monitoring.
18:07:48 Stopped logcat recording.
18:07:48 Done. Test time = 12 (secs)
18:07:48 Starting results processing. Attempt: 1
18:08:01 Completed results processing. Time taken = 9 (secs)
18:08:01 Test is Finished

Instrumentation testing complete.

More details are available at [https://console.firebase.google.com/project/*********************/testlab/histories/bh.73ae3a33c4730f39/matrices/7660637650631969176].
┌─────────┬───────────────────────┬──────────────────────┐
│ OUTCOME │    TEST_AXIS_VALUE    │     TEST_DETAILS     │
├─────────┼───────────────────────┼──────────────────────┤
│ Passed  │ Pixel2-27-en-portrait │ 12 test cases passed │
└─────────┴───────────────────────┴──────────────────────┘

CircleCI")

(test parse-firebase-output
  (let ((res (parse-firebase-output *output*)))
    (is (equal "cloud-test-screenshotbot-example"
               (firebase-output-bucket res)))))
