;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot.sdk.test-sdk
  (:use :cl
   :alexandria
        :fiveam)
  (:import-from :screenshotbot-sdk
   :%read-directory-from-args
                :*directory*
   :directory-image-bundle
                :*metadata*
                :get-relative-path
                :*ios-diff-dir*
                :image-directory
                :image-directory-with-diff-dir
   :*metadata*
   :*ios-diff-dir*))
(in-package :screenshotbot.sdk.test-sdk)

(def-suite* :screenshotbot.sdk.test-sdk)

(test read-directory-for-ios
  (tmpdir:with-tmpdir (s)
    (let ((*directory* (namestring s)))
     (is (typep (%read-directory-from-args)
                'image-directory)))))

(test read-directory-for-android
  (tmpdir:with-tmpdir (s)
    (let ((*directory* (namestring s)))
      (uiop:with-temporary-file (:pathname metadata :type "xml")
        (let ((*metadata* (namestring metadata)))
          (is (typep (%read-directory-from-args)
                     'directory-image-bundle)))))))

(test read-directory-with-ios-diff-dir
  (tmpdir:with-tmpdir (s)
    (tmpdir:with-tmpdir (s2)
      (let ((*directory* (namestring s))
            (*ios-diff-dir* (namestring s2)))
        (is (typep (%read-directory-from-args)
                   'image-directory-with-diff-dir))))))

(test get-relative-path
  (is (equal #P "foo/"
             (get-relative-path #P "/bar/car/foo/" "/bar/car/")))
  (is (equal #P "foo/dar/"
             (get-relative-path #P "/bar/car/foo/dar/" "/bar/car/"))))
