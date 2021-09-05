;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-sdk
  (:use :cl
   :alexandria
        :fiveam)
  (:import-from #:screenshotbot/sdk/sdk
                #:%read-directory-from-args
                #:*directory*
                #:*metadata*
                #:get-relative-path
                #:*ios-diff-dir*
                #:*metadata*
                #:*ios-diff-dir*
                #:put-file)
  (:import-from #:screenshotbot/sdk/android
                #:directory-image-bundle)
  (:import-from #:screenshotbot/sdk/bundle
                #:image-directory
                #:image-directory-with-diff-dir))
(in-package :screenshotbot/sdk/test-sdk)

(util/fiveam:def-suite)

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


(hunchentoot:define-easy-handler (get-md5-sum :uri "/put" :acceptor-names '(test-acceptor)) ()
  (declare (optimize (speed 0) (debug 3)))
  (screenshotbot/api/image:with-raw-post-data-as-tmp-file (p)
    (ironclad:byte-array-to-hex-string (md5:md5sum-file p))))

(defmacro with-acceptor ((acceptor) &body body)
  `(let ((acceptor ,acceptor)
         (fn (lambda () ,@body)))
     (unwind-protect
          (progn
            (hunchentoot:start acceptor)
            (setf hunchentoot:*catch-errors-p* nil)
            (funcall fn))
       (progn
         (setf hunchentoot:*catch-errors-p* t)
         (hunchentoot:stop acceptor)))))

#-darwin
(test simple-put-image
  (let* ((port (util:random-port))
         (acceptor (make-instance 'hunchentoot:easy-acceptor
                                  :port port
                                  :name 'test-acceptor)))
    (log:info "Using port: ~a" (hunchentoot:acceptor-port acceptor))
    (with-acceptor (acceptor)
     (with-open-file (s (asdf:system-relative-pathname
                         :screenshotbot.sdk
                         "file-for-test.bin")
                        :direction :input
                        :element-type 'flexi-streams:octet)
       (is
        (equal
         "4249fe0e72f21fd54dbb2f3325bec263"
         (put-file (format nil "http://127.0.0.1:~a/put" port)
                   s)))))))
