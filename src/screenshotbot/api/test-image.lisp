;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/api/test-image
  (:use #:cl
        #:alexandria
        #:bknr.datastore
        #:fiveam)
  (:import-from #:util
                #:oid)
  (:import-from #:screenshotbot/api/image
                #:verify-and-upload-from-path
                #:upload-response-upload-url
                #:upload-response-id
                #:prepare-upload-api-api-handler
                #:prepare-upload-api
                #:*use-blob-store-p*
                #:*build-presigned-put*)
  (:import-from #:screenshotbot/model/company
                #:verified-p
                #:company)
  (:import-from #:screenshotbot/model/user
                #:user)
  (:import-from #:screenshotbot/model/image
                #:with-local-image
                #:image)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/installation
                #:*installation*
                #:installation
                #:multi-org-feature)
  (:import-from #:screenshotbot/api/core
                #:api-error))

(util/fiveam:def-suite)

(defclass my-installation (multi-org-feature
                           installation)
  ())

(def-fixture state ()
  (let ((*installation* (make-instance 'my-installation)))
   (with-test-store ()
     (with-fake-request ()
       (with-test-user (:company company
                        :user user
                        :api-key api-key
                        :logged-in-p t)
         (let ((*build-presigned-put* (lambda (bucket key) "https://example.com")))
           (&body)))))))

(test simple-upload
  (with-fixture state ()
    (let ((response (prepare-upload-api
                 :hash "abcd"
                 :content-type "image/png")))
      (is (not (str:emptyp (upload-response-id response))))
      (is-true (upload-response-upload-url response)))))

(test reupload-same-url
  (with-fixture state ()
    (let ((old-im (make-image :hash "abcd"
                              :company company
                              :verified-p t)))
     (let ((response (prepare-upload-api
                  :hash "abcd"
                  :content-type "image/png")))
       (is (equal (oid old-im)
                  (upload-response-id response)))
       (is-false (upload-response-upload-url response))))))

(test reupload-unverified-image
  (with-fixture state ()
    (let ((old-im (make-image :hash "abcd"
                              :company company
                              :verified-p nil)))
     (let ((response (prepare-upload-api
                      :hash "abcd"
                      :content-type "image/png")))
       (is (not (equal (oid old-im)
                       (upload-response-id response))))
       (is-true (upload-response-upload-url response))))))

(test verify-and-upload-from-path-happy-path
  (with-fixture state ()
    (let ((im (make-image :hash "3858f62230ac3c915f300c664312c63f"
                          :company company
                          :verified-p nil)))
      (uiop:with-temporary-file (:pathname p :stream s :direction :output)
        (write-string "foobar" s)
        (finish-output s)
        (is-false (verified-p im))
        (is
         (equal "0"
                (verify-and-upload-from-path im p)))
        (is-true (verified-p im))
        (with-local-image (file im)
          (is (equal "foobar"
                     (uiop:read-file-string file))))))))

(test verify-and-upload-from-path-when-file-doesnt-match
  (with-fixture state ()
    (let ((im (make-image :hash "3858f62230ac3c915f300c664312c63f"
                          :company company
                          :verified-p nil)))
      (uiop:with-temporary-file (:pathname p :stream s :direction :output)
        (write-string "carbar" s) ;; the actual image should be "foobar"
        (finish-output s)
        (is-false (verified-p im))
        (signals api-error
          (verify-and-upload-from-path im p))
        (is-false (verified-p im))))))
