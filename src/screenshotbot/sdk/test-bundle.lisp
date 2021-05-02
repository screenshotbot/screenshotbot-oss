;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot.sdk.test-bundle
  (:use :cl
   :alexandria
        :fiveam)
  (:import-from :screenshotbot-sdk
   :image-directory
   :image-directory-with-diff-dir
   :image-name
   :image-pathname
   :list-images))
(in-package :screenshotbot.sdk.test-bundle)

(def-suite* :screenshotbot.sdk.test-bundle)

(defun %set-equal (x y)
  (set-equal x y :test 'equalp))

(def-fixture state ()
  (tmpdir:with-tmpdir (dir)
    (with-open-file (s1 (path:catfile dir "foo1.png")  :direction :output)
      (with-open-file (s2 (path:catfile dir "foo2.txt") :direction :output)
        (with-open-file (s3 (path:catfile dir "foo3.png") :direction :output)
          (&body))))))

(test test-bundle
  (with-fixture state ()
    (let ((bundle (make-instance 'image-directory
                                 :directory dir)))
      (let ((ims (list-images bundle)))
        (is (eql 2 (length ims)))
        (is (%set-equal
             (list "foo1" "foo3")
             (mapcar 'image-name ims)))))))

(test diff-dir
  (with-fixture state ()
    (tmpdir:with-tmpdir (diff-dir)
      (with-open-file (s3-fail (path:catfile diff-dir "failed_foo3.png") :direction :output)
        (let ((bundle (make-instance 'image-directory-with-diff-dir
                                     :directory dir
                                     :diff-dir diff-dir)))
          (let ((ims (list-images bundle)))
            (is (%set-equal
                 (list "foo1" "foo3")
                 (mapcar 'image-name ims)))
            (is (%set-equal
                 (mapcar 'namestring
                         (list (path:catfile dir "foo1.png")
                               (path:catfile diff-dir "failed_foo3.png")))
                 (mapcar 'namestring (mapcar 'image-pathname ims))))))))))
