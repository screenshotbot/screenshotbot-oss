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
                #:validate-pull-request
                #:backoff
                #:parse-environment
                #:%read-directory-from-args
                #:*directory*
                #:*metadata*
                #:get-relative-path
                #:*ios-diff-dir*
                #:*metadata*
                #:*ios-diff-dir*
                #:put-file)
  (:import-from #:screenshotbot/sdk/android
                #:image-bundles
                #:directory-image-bundle)
  (:import-from #:screenshotbot/sdk/bundle
                #:image-directory
                #:image-directory-with-diff-dir)
  (:import-from #:util/digests
                #:md5-file)
  (:import-from #:util/testing
                #:with-local-acceptor)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags)
                    (#:a #:alexandria)))
(in-package :screenshotbot/sdk/test-sdk)

(util/fiveam:def-suite)


(def-fixture state ()
  (cl-mock:with-mocks ()
    (let ((auto-restart:*global-enable-auto-retries-p* nil))
     (util:copying (flags:*pull-request*
                    flags:*override-commit-hash*
                    flags:*main-branch*)
       (&body)))))

(test read-directory-for-ios
  (with-fixture state ()
   (tmpdir:with-tmpdir (s)
     (let ((*directory* (namestring s)))
       (is (typep (%read-directory-from-args)
                  'image-directory))))))

(test read-directory-for-android
  (with-fixture state ()
   (tmpdir:with-tmpdir (s)
     (let ((*directory* (namestring s)))
       (uiop:with-temporary-file (:pathname metadata :type "json")
         (let ((*metadata* (namestring metadata)))
           (is (typep (car (image-bundles (%read-directory-from-args)))
                      'directory-image-bundle))))))))

(test read-directory-with-ios-diff-dir
  (with-fixture state ()
   (tmpdir:with-tmpdir (s)
     (tmpdir:with-tmpdir (s2)
       (let ((*directory* (namestring s))
             (*ios-diff-dir* (namestring s2)))
         (is (typep (%read-directory-from-args)
                    'image-directory-with-diff-dir)))))))

(test get-relative-path
  (is (equal #P "foo/"
             (get-relative-path #P "/bar/car/foo/" "/bar/car/")))
  (is (equal #P "foo/dar/"
             (get-relative-path #P "/bar/car/foo/dar/" "/bar/car/"))))


(hunchentoot:define-easy-handler (get-md5-sum :uri "/put" :acceptor-names '(test-acceptor)) ()
  (log:info "Running fake PUT")
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :md5
    (hunchentoot:raw-post-data :force-binary t
                               :want-stream nil))))


#-darwin
(test simple-put-image
  (with-fixture state ()
   (with-local-acceptor (host) ('hunchentoot:easy-acceptor
                                 :name 'test-acceptor)
     (with-open-file (s (asdf:system-relative-pathname
                         :screenshotbot.sdk
                         "file-for-test.bin")
                        :direction :input
                        :element-type 'flexi-streams:octet)
       (is
        (equal
         "4249fe0e72f21fd54dbb2f3325bec263"
         (put-file (format nil "~a/put" host) s)))))))

(defvar *env-overrides* nil)

(defun fake-getenv (name)
  (a:assoc-value *env-overrides* name :test #'equal))

(defmacro with-env (bindings &body body)
  `(util:copying (*env-overrides*)
     ,@ (loop for (name var) in bindings
              collect
              `(push (cons (str:replace-all "-" "_" (string ,name)) ,var)
                     *env-overrides*))
     (cl-mock:if-called 'uiop:getenv #'fake-getenv)
     ,@body))

(test parse-override-commit-hash
  (with-fixture state ()
    (with-env ((:circle-pull-request "http://foo"))
      (parse-environment)
      (is (equal "http://foo" flags:*pull-request*))))
  (with-fixture state ()
    (with-env ((:circle-sha1 "abcd")
               (:circle-pull-request "http://foo"))
      (parse-environment)
      (is (equal "abcd" flags:*override-commit-hash*))))
  (with-fixture state ()
    (with-env ((:circle-sha1 "abcd")
               (:circle-pull-request ""))
      (parse-environment)
      (is (equal nil flags:*override-commit-hash*)))))

(test parse-override-commit-hash-with-bitrise
    (with-fixture state ()
    (with-env ((:bitrise-git-commit "abcd")
               (:bitriseio-pull-request-repository-url "https://bitbucket.org/tdrhq/fast-example"))
      (parse-environment)
      (is (equal nil flags:*pull-request*))
      (is (equal nil flags:*override-commit-hash*))))

  (with-fixture state ()
    (with-env ((:bitrise-git-commit "abcd")
               (:bitrise-pull-request "1")
               (:bitriseio-pull-request-repository-url "https://bitbucket.org/tdrhq/fast-example"))
      (parse-environment)

      (is (equal "https://bitbucket.org/tdrhq/fast-example/pull-requests/1"
                 flags:*pull-request*))

      (is (equal "abcd" flags:*override-commit-hash*)))))

(test backoff-happy-path
  (is (eql 20 (backoff 1)))
  (is (eql 60 (backoff 100))))

(test validate-pull-request
  (with-fixture state ()
    (flet ((%run (url)
             (setf flags:*pull-request* url)
             (validate-pull-request)
             flags:*pull-request*))
      (handler-case
          (%run nil)
        (simple-warning ()
          (fail "Saw warning for nil")))
      (signals simple-warning
        (%run "git@github.com:trhq/fast-example.git"))
      (is (equal nil (%run "git@github.com:tdrhq/fast-example.git")))
      (is (equal "https://github.com/tdrhq/fast-example/pulls/1" (%run "https://github.com/tdrhq/fast-example/pulls/1"))))))
