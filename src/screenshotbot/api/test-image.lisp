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
  (:import-from #:../user-api
                #:*current-api-key*)
  (:import-from #:util
                #:oid)
  (:import-from #:./image
                #:prepare-upload-api
                #:*use-blob-store-p*
                #:*build-presigned-put*)
  (:import-from #:../model/company
                #:company)
  (:import-from #:../model/user
                #:user)
  (:import-from #:../model/image
                #:image)
  (:import-from #:../testing
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
                #:multi-org-feature))

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
                        :api-key api-key)
         (let ((*current-api-key* api-key)
               (*use-blob-store-p* nil)
               (*build-presigned-put* (lambda (bucket key) "https://example.com")))
           (&body)))))))

(test simple-upload
  (with-fixture state ()
    (let ((resp (json:decode-json-from-string
                 (prepare-upload-api
                  :hash "abcd"
                  :content-type "image/png"))))
      (let ((response (assoc-value resp :response)))
        (is (not (str:emptyp (assoc-value response :id))))
        (is-true (assoc-value response :upload-url))))))

(test reupload-same-url
  (with-fixture state ()
    (let ((old-im (make-image :hash "abcd"
                              :company company
                              :verified-p t)))
     (let ((resp (json:decode-json-from-string
                  (prepare-upload-api
                   :hash "abcd"
                   :content-type "image/png"))))
       (let ((response (assoc-value resp :response)))
         (is (equal (oid old-im) (assoc-value response :id)))
         (is-false (assoc-value response :upload-url)))))))

(test reupload-unverified-image
  (with-fixture state ()
    (let ((old-im (make-image :hash "abcd"
                              :company company
                              :verified-p nil)))
     (let ((resp (json:decode-json-from-string
                  (prepare-upload-api
                   :hash "abcd"
                   :content-type "image/png"))))
       (let ((response (assoc-value resp :response)))
         (is (not (equal (oid old-im) (assoc-value response :id))))
         (is-true (assoc-value response :upload-url)))))))
