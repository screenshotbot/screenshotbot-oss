;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/test-replay-acceptor
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/replay/replay-acceptor
                #:acceptor-snapshots
                #:asset-maps
                #:pop-snapshot
                #:snapshots-company
                #:handle-asset
                #:handle-asset-from-company
                #:push-snapshot
                #:render-acceptor
                #:default-render-acceptor
                #:*default-render-acceptor*)
  (:import-from #:util/testing
                #:with-local-acceptor)
  (:import-from #:screenshotbot/replay/core
                #:http-header
                #:assets
                #:uuid
                #:snapshot
                #:asset)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/object-id
                #:oid-array)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/replay/test-replay-acceptor)

(util/fiveam:def-suite)

(defparameter *fixture* (asdf:system-relative-pathname
                         :screenshotbot
                         "fixture/rose.png"))

(def-fixture state (&key response-headers)
  (with-test-store (:globally t)
    (tmpdir:with-tmpdir (tmpdir)
     (with-local-acceptor (host :acceptor acceptor)
         ('render-acceptor)
       (fad:copy-file *fixture* (path:catfile tmpdir "abcd00.png"))
       (let* ((*default-render-acceptor* acceptor)
              (company (make-instance 'company))
              (snapshot (make-instance 'snapshot
                                        :tmpdir tmpdir))
              (asset (make-instance
                      'asset
                       :status 200
                       :url "https://www.example.com"
                       :file (format nil "/snapshot/~a/assets/abcd00.png"
                                     (uuid snapshot))
                       :response-headers
                       (list* (make-instance 'http-header
                                             :name "Content-type"
                                             :value "image/png")
                              response-headers))))
         (setf (assets snapshot)
               (list asset))
         (&body))))))

(test simple-loading
  (with-fixture state ()
    (push-snapshot acceptor company snapshot)
    (let ((url (format nil "~a/snapshot/~a/assets/abcd00.png"
                       host
                       (uuid snapshot))))
      (multiple-value-bind (stream ret)
          (dex:get url
                   :force-binary t
                   :want-stream t)
        (with-open-stream (stream stream)
          (is (equal 200 ret))
          (is (equalp (md5:md5sum-file *fixture*)
                      (md5:md5sum-stream stream))))))))


(test expect-404-for-non-existent-assets
  (with-fixture state ()
    (push-snapshot acceptor company snapshot)
    (let ((url (format nil "~a/snapshot/~a/assets/abcd01.png"
                       host
                       (uuid snapshot))))
      (signals dex:http-request-not-found
        (dex:get url)))))

(test loading-by-company
  (with-fixture state ()
    (push-snapshot acceptor company snapshot)
    (let ((url (format nil "~a/company/~a/assets/abcd00.png"
                       host
                       (encrypt:encrypt-mongoid (oid-array company)))))
      (multiple-value-bind (stream ret headers)
          (dex:get url
                   :force-binary t
                   :want-stream t)
        (with-open-stream (stream stream)
          (is (equal 200 ret))
          (is (equalp (md5:md5sum-file *fixture*)
                      (md5:md5sum-stream stream))))

        ;; verify minimum caching
        (is (equal "max-age=300" (gethash "cache-control" headers)))))))

(test loading-by-company
  (with-fixture state ()
    (push-snapshot acceptor company snapshot)
    (let ((url (format nil "~a/company/~a/assets/abcd00.png"
                       host
                       (encrypt:encrypt-mongoid (oid-array company)))))
      (multiple-value-bind (stream ret headers)
          (dex:get url
                   :force-binary t
                   :want-stream t)
        (with-open-stream (stream stream)
          (is (equal 200 ret))
          (is (equalp (md5:md5sum-file *fixture*)
                      (md5:md5sum-stream stream))))

        ;; verify minimum caching
        (is (equal "max-age=300" (gethash "cache-control" headers)))))))

(test http-revalidate-by-HEAD
  (with-fixture state ()
    (push-snapshot acceptor company snapshot)
    (let ((url (format nil "~a/company/~a/assets/abcd00.png"
                       host
                       (encrypt:encrypt-mongoid (oid-array company)))))
      (multiple-value-bind (data ret headers)
          (dex:head url)
        (is (equalp #() data))
        (is (equal 200 ret))

        ;; verify minimum caching
        (is (equal "max-age=300" (gethash "cache-control" headers)))))))

(test dont-override-max-age-for-large
  (with-fixture state (:response-headers (list (make-instance 'http-header
                                                               :name "Cache-Control"
                                                               :value "max-age=360000")))
    (push-snapshot acceptor company snapshot)
    (let ((url (format nil "~a/company/~a/assets/abcd00.png"
                       host
                       (encrypt:encrypt-mongoid (oid-array company)))))
      (multiple-value-bind (stream ret headers)
          (dex:get url
                   :force-binary t
                   :want-stream t)
        (with-open-stream (stream stream)
          (is (equal 200 ret))
          (is (equalp (md5:md5sum-file *fixture*)
                      (md5:md5sum-stream stream))))

        ;; max-age should not be overwritten
        (is (equal "max-age=360000" (gethash "cache-control" headers)))))))

(test small-max-ages-are-overwritten
  (with-fixture state (:response-headers (list (make-instance 'http-header
                                                               :name "Cache-Control"
                                                               :value "max-age=2")))
    (push-snapshot acceptor company snapshot)
    (let ((url (format nil "~a/company/~a/assets/abcd00.png"
                       host
                       (encrypt:encrypt-mongoid (oid-array company)))))
      (multiple-value-bind (stream ret headers)
          (dex:get url
                   :force-binary t
                   :want-stream t)
        (with-open-stream (stream stream)
          (is (equal 200 ret))
          (is (equalp (md5:md5sum-file *fixture*)
                      (md5:md5sum-stream stream))))

        ;; max-age should not be overwritten
        (is (equal "max-age=300" (gethash "cache-control" headers)))))))

(test expect-404-for-non-existent-assets-by-company
  (with-fixture state ()
    (let ((url (format nil "~a/company/~a/assets/abcd01.png"
                       host
                       (encrypt:encrypt-mongoid (oid-array company)))))
      (handler-case
          (progn
            (dex:get url)
            (fail "expected error"))
        (dex:http-request-not-found (e)
          (is (equal "max-age=60" (gethash "cache-control" (dex:response-headers e)))))))))

(test expect-404-for-non-existent-file
  (with-fixture state ()
    (let ((url (format nil "~a/foo/bar.png"
                       host)))
      (handler-case
          (progn
            (dex:get url)
            (fail "expected error"))
        (dex:http-request-not-found (e)
          (is (equal "max-age=3600" (gethash "cache-control" (dex:response-headers e)))))))))


(test handle-asset-from-company
  (with-fixture state ()
    (push-snapshot acceptor company snapshot)
    (cl-mock:with-mocks ()
     (let ((called-args nil))
       (cl-mock:if-called 'handle-asset
                           (lambda (snapshot asset)
                             (setf called-args (list snapshot asset))))
       (handle-asset-from-company acceptor company "abcd00.png")
       (is (equal (list snapshot asset)
                  called-args))))))

(test cleanup-pop-snapshot
  (with-fixture state ()
    (is (eql 0 (length (snapshots-company acceptor))))
    (push-snapshot acceptor company snapshot)
    (is (eql 1 (length (snapshots-company acceptor))))
    (pop-snapshot acceptor snapshot)
    (is (eql 0 (length (snapshots-company acceptor))))
    (is (eql '()
              (a:hash-table-keys (asset-maps acceptor))))
    (is (eql '()
              (a:hash-table-keys (acceptor-snapshots acceptor))))))
