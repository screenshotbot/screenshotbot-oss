;; -*- coding: utf-8 -*-
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
                #:put-run
                #:single-directory-run
                #:format-api-url
                #:empty-run-error
                #:make-run
                #:request
                #:backoff
                #:make-bundle
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
  (:import-from #:cl-mock
                #:answer
                #:if-called)
  (:import-from #:screenshotbot/sdk/git
                #:current-branch
                #:cleanp
                #:merge-base
                #:current-commit
                #:rev-parse)
  (:import-from #:screenshotbot/api/model
                #:*api-version*
                #:decode-json)
  (:import-from #:util/json-mop
                #:json-mop-to-string)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-work-branch)
  (:import-from #:hunchentoot
                #:easy-acceptor
                #:define-easy-handler)
  (:import-from #:util/misc
                #:with-global-binding)
  (:import-from #:screenshotbot/sdk/api-context
                #:remote-version
                #:api-context)
  (:import-from #:screenshotbot/sdk/run-context
                #:run-context)
  (:import-from #:screenshotbot/api/commit-graph
                #:update-commit-graph)
  (:local-nicknames (#:flags #:screenshotbot/sdk/flags)
                    (#:a #:alexandria)
                    (#:dto #:screenshotbot/api/model)))
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
       (is (typep (make-bundle)
                  'image-directory))))))

(test read-directory-for-android
  (with-fixture state ()
   (tmpdir:with-tmpdir (s)
     (let ((*directory* (namestring s)))
       (uiop:with-temporary-file (:pathname metadata :type "json")
         (let ((*metadata* (namestring metadata)))
           (is (typep (car (image-bundles (make-bundle)))
                      'directory-image-bundle))))))))

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
     (with-open-file (s #.(asdf:system-relative-pathname
                           :screenshotbot.sdk
                           "file-for-test.bin")
                        :direction :input
                        :element-type 'flexi-streams:octet)
       (is
        (equal
         "4249fe0e72f21fd54dbb2f3325bec263"
         (put-file (make-instance 'api-context
                                  :key ""
                                  :secret ""
                                  :hostname host)
                   (format nil "~a/put" host) s)))))))

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

(test backoff-happy-path
  (is (eql 10 (backoff 1)))
  (is (eql nil (backoff 100))))



(test make-run-happy-path
  "Does not test much, sadly"
  (with-fixture state ()
    (let ((%content))
      (if-called 'request
                 (lambda (api-context uri &key method content)
                   (setf %content content)
                   nil))
      (let ((repo :dummy-repo))
        (answer (rev-parse repo "main") "abcd")
        (answer (current-commit repo) "bdfd")
        (answer (current-branch repo) "my-branch")
        (answer (merge-base "abcd" "bdfd" "abcd"))
        (answer (cleanp repo) t)
        (let ((context (make-instance 'api-context
                                      :remote-version *api-version*)))
          (finishes
            (make-run context
                      `(((:id . "foo")
                         (:name . "car")))
                      (make-instance 'run-context
                                     :main-branch "main"
                                     :repo-clean-p t
                                     :repo-url repo)))
          (is-true (typep %content 'dto:run)))))))

(test make-run-on-empty-directory-crashes-appropriately
  (with-fixture state  ()
    (if-called 'request
               (lambda (&rest args)
                 (error "should not be called")))
    (let ((api-context (make-instance 'api-context)))
     (signals empty-run-error
       (make-run api-context nil
                 (make-instance 'run-context
                                :main-branch "main"))))))

#+lispworks
(test |ensure we're not depending on cl+ssl|
  (let ((seen (make-hash-table :test #'equal)))
    (labels ((find-bad (component path)
               (when (string-equal "cl+ssl" (asdf:component-name component))
                 (fail "Found cl+ssl in deps ~S" path))
               (unless (gethash component seen)
                 (setf (gethash component seen) t)
                 (dolist (dep (asdf:system-depends-on component))
                   (unless (consp dep)
                     (find-bad (asdf:find-system dep)
                               (list* dep path)))))))
      (find-bad (asdf:find-system :screenshotbot.sdk) nil))))

(test encode-decode-utf-8-branchname
  (let ((run (make-instance 'dto:run :work-branch "léquipe")))
    (let ((json (decode-json (json-mop-to-string run) 'dto:run)))
      (is (equal "léquipe" (dto:work-branch run))))))

(defvar *result* nil)

(define-easy-handler (test-handler :uri (lambda (request) request) :acceptor-names '(test-handler)) ()
  (setf *result* (json:decode-json-from-string (hunchentoot:raw-post-data :force-text t)))
  "{}")

(test make-request-with-unicode
  (with-global-binding ((*result* nil)
                        (auto-restart:*global-enable-auto-retries-p* nil))
    (with-local-acceptor (host) ('easy-acceptor :name 'test-handler)
      (let ((api-context (make-instance 'api-context
                                        :remote-version *api-version*
                                        :key ""
                                        :secret ""
                                        :hostname host)))
        (finishes
          (request api-context
                   "/test-handler"
                   :method :put
                   :content (make-instance 'dto:report :id "foobar")))
        (is (equal "foobar" (assoc-value *result* :id)))
        (finishes
          (request api-context
                   "/test-handler"
                   :method :put
                   :content (make-instance 'dto:report :id "léquipe")))
        (is (equal "léquipe" (assoc-value *result* :id)))))))

(test format-api-url
  (is (equal "https://api.screenshotbot.io/api/version"
             (format-api-url
              (make-instance 'api-context
                             :hostname "https://api.screenshotbot.io")
              "/api/version")))
  (is (equal "https://foo.screenshotbot.io/api/version"
             (format-api-url
              (make-instance 'api-context
                             :hostname "https://foo.screenshotbot.io/")
              "/api/version")))
  (is (equal "https://foo.screenshotbot.io/api/version"
             (format-api-url
              (make-instance 'api-context
                             :hostname "foo.screenshotbot.io")
              "/api/version"))))


(test single-directory-run-happy-path
  (with-fixture state ()
    (if-called 'request
               (lambda (context request &key parameters)
                 nil))
    (if-called 'remote-version
               (lambda (context)
                 *api-version*))
    (if-called 'put-run
               (lambda (context run)
                 nil))
    (tmpdir:with-tmpdir (dir)
      (with-open-file (stream (path:catfile dir "foo.png") :direction :output))
      (single-directory-run
       :api-context
       (make-bundle :directory (namestring dir))
       (make-instance 'run-context
                      :main-branch "master"
                      :repo-clean-p t
                      :productionp nil)))))
