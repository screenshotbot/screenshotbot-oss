;; -*- coding: utf-8 -*-
;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-sdk
  (:use #:cl
        #:fiveam)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:cl-mock
                #:answer
                #:if-called)
  (:import-from #:fiveam-matchers/core
                #:assert-that
                #:is-equal-to)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/strings
                #:is-not-empty)
  (:import-from #:hunchentoot
                #:define-easy-handler
                #:easy-acceptor)
  (:import-from #:screenshotbot/api/model
                #:*api-version*
                #:decode-json)
  (:import-from #:screenshotbot/sdk/android
                #:directory-image-bundle
                #:image-bundles)
  (:import-from #:screenshotbot/sdk/api-context
                #:api-context)
  (:import-from #:screenshotbot/sdk/backoff
                #:backoff)
  (:import-from #:screenshotbot/sdk/bundle
                #:image-directory)
  (:import-from #:screenshotbot/sdk/flags
                #:*directory*
                #:*metadata*)
  (:import-from #:screenshotbot/sdk/git
                #:cleanp
                #:current-branch
                #:current-commit
                #:merge-base
                #:null-repo
                #:repo-link
                #:rev-parse)
  (:import-from #:screenshotbot/sdk/hostname
                #:format-api-url)
  (:import-from #:screenshotbot/sdk/integration-fixture
                #:with-sdk-integration)
  (:import-from #:screenshotbot/sdk/sdk
                #:maybe-retry-request
                #:%request
                #:empty-run-error
                #:find-existing-images
                #:get-relative-path
                #:invalid-pull-request
                #:keyword-except-md5
                #:make-bundle
                #:make-directory-run
                #:make-run
                #:not-recent-file-warning
                #:parse-environment
                #:put-file
                #:request
                #:upload-image-directory
                #:validate-pull-request
                #:warn-if-not-recent-file)
  (:import-from #:util/json-mop
                #:json-mop-to-string)
  (:import-from #:util/misc
                #:with-global-binding)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:util/testing
                #:with-local-acceptor)
  (:local-nicknames (#:a #:alexandria)
                    (#:api-key #:core/api/model/api-key)
                    (#:dto #:screenshotbot/api/model)
                    (#:flags #:screenshotbot/sdk/flags)
                    (#:run-context #:screenshotbot/sdk/run-context)))
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
      (is (equal "abcd" flags:*override-commit-hash*)))))

(test parse-override-commit-hash-with-bitrise
    (with-fixture state ()
    (with-env ((:bitrise-git-commit "abcd")
               (:bitriseio-pull-request-repository-url "https://bitbucket.org/tdrhq/fast-example"))
      (parse-environment)
      (is (equal nil flags:*pull-request*))
      (is (equal "abcd" flags:*override-commit-hash*))))

  (with-fixture state ()
    (with-env ((:bitrise-git-commit "abcd")
               (:bitrise-pull-request "1")
               (:bitriseio-pull-request-repository-url "https://bitbucket.org/tdrhq/fast-example"))
      (parse-environment)

      (is (equal "https://bitbucket.org/tdrhq/fast-example/pull-requests/1"
                 flags:*pull-request*))

      (is (equal "abcd" flags:*override-commit-hash*)))))

(test backoff-happy-path
  (is (eql 10 (backoff 1)))
  (is (eql nil (backoff 100))))

(test validate-pull-request
  (with-fixture state ()
    (flet ((%run (url)
             (setf flags:*pull-request* url)
             (validate-pull-request)
             flags:*pull-request*))
      (handler-case
          (%run nil)
        (invalid-pull-request ()
          (fail "Saw warning for nil")))
      (signals invalid-pull-request
        (%run "git@github.com:trhq/fast-example.git"))
      (is (equal nil (%run "git@github.com:tdrhq/fast-example.git")))
      (is (equal "https://github.com/tdrhq/fast-example/pulls/1" (%run "https://github.com/tdrhq/fast-example/pulls/1"))))))

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
        (if-called 'merge-base
                   (lambda (git-repo main-branch commit-hash)
                     "0001"))
        (answer (repo-link repo) "https://github.com/tdrhq/fast-example")
        (answer (cleanp repo) t)
        (let ((context (make-instance 'api-context
                                      :remote-version *api-version*)))
          (finishes
            (make-run context
                      (list
                       (make-instance 'dto:screenshot
                                      :image-id "foo"
                                      :name "car"))
                      :branch "main"
                      :repo repo))
          (is-true (typep %content 'dto:run))
          (is (equal "0001" (dto:merge-base %content)))
          (is (equal "bdfd" (dto:run-commit %content)))
          (is (equal "https://github.com/tdrhq/fast-example" (dto:run-repo %content))))))))


(test make-run-with-context-uses-information-from-the-context
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
        (if-called 'merge-base
                   (lambda (git-repo main-branch commit-hash)
                     "0001"))
        (answer (repo-link repo) "https://github.com/tdrhq/fast-example")
        (answer (cleanp repo) t)
        (let ((context (make-instance 'api-context
                                      :remote-version *api-version*)))
          (finishes
            (make-run context
                      (list
                       (make-instance 'dto:screenshot
                                      :image-id "foo"
                                      :name "car"))
                      :branch "main"
                      :repo repo
                      :run-context (make-instance 'run-context:run-context
                                                  :commit-hash "0002"
                                                  :merge-base "0003"
                                                  :repo-url "https://example.com/foo.git")))
          (is-true (typep %content 'dto:run))
          (is (equal "0003" (dto:merge-base %content)))
          (is (equal "0002" (dto:run-commit %content)))
          (is (equal "https://example.com/foo.git" (dto:run-repo %content))))))))

(test make-run-on-empty-directory-crashes-appropriately
  (with-fixture state  ()
    (if-called 'request
               (lambda (&rest args)
                 (error "should not be called")))
    (let ((api-context (make-instance 'api-context)))
     (signals empty-run-error
       (make-run api-context nil :branch "main")))))

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

(test retries-%request
  (with-fixture state ()
    (let ((count 0))
     (cl-mock:if-called 'http-request
                        (lambda (url &rest args)
                          (incf count)
                          (cond
                            ((<= count 3)
                             (values (make-string-input-stream "Bad") 502))
                            (t
                             (values (make-string-input-stream "Good") 200)))))
      (is
       (equal "Good"
              (%request (make-instance 'api-context
                                       :remote-version *api-version*
                                       :key "foo"
                                       :secret "bar"
                                       :hostname "https://example.com")
                        "/api/test"
                        :backoff 0))))))

(test maybe-retry-request-happy-path
  "Just that for each of the codes, we correctly invoke the restart,
since the logs might be different for each code."
  (loop for code in '(429 502 503) do
    (is
     (eql :done
          (restart-case
              (maybe-retry-request code
                                   :attempt 0
                                   :backoff 0
                                   :restart 'foobar)
            (foobar ()
              :done))))))

(test retries-%request-will-finally-fail
  (with-fixture state ()
    (let ((count 0))
     (cl-mock:if-called 'http-request
                        (lambda (url &rest args)
                          (incf count)
                          (cond
                            ((<= count 10)
                             (values (make-string-input-stream "Bad") 502))
                            (t
                             (values (make-string-input-stream "Good") 200)))))
      (is
       ;; See TODO in %request, this should probably signal an error
       (equal "Bad"
              (%request (make-instance 'api-context
                                       :remote-version *api-version*
                                       :key "foo"
                                       :secret "bar"
                                       :hostname "https://example.com")
                        "/api/test"
                        :backoff 0))))))

(test warn-if-not-recent-file
  (uiop:with-temporary-file (:stream s)
    (handler-case
        (warn-if-not-recent-file s)
      (warning ()
        (fail "expected exception"))
      (:no-error (_)
        (pass)))

    #+linux ;; mostly for the `touch`
    (uiop:with-temporary-file (:stream s :pathname p)
      (uiop:run-program
       (list "touch" "-d" "2 days ago" (namestring p)))
      (signals not-recent-file-warning
        (warn-if-not-recent-file s)))))

(test format-not-recent-file-warning
  (finishes
   (format nil "~a"
           (make-condition 'not-recent-file-warning :file "/tmp/foo"))))

(defun write-string-to-file (str file)
  (with-open-file (s file :direction :output)
    (write-string str s)
    (finish-output s)))


(test find-existing-images-happy-path
  (with-fixture state ()
    (with-sdk-integration (api-context)
      (let ((result
             (find-existing-images api-context
                                   #("82142ae81caba45bb76aa21fb6acf16d"))))
        (assert-that result
                     (has-length 1))

        (assert-that (dto:image-md5sum (car result))
                     (is-equal-to "82142ae81caba45bb76aa21fb6acf16d"))

        (assert-that
         (dto:image-upload-url (car result))
         (is-not-empty))
        (assert-that
         (dto:image-id (car result))
         (is-not-empty))))))

(test simple-image-loading-happy-path
  (with-fixture state ()
    (with-sdk-integration (api-context)
      (tmpdir:with-tmpdir (dir)
        (write-string-to-file "foobar" (path:catfile dir "one.png"))
        (write-string-to-file "foobar2" (path:catfile dir "two.png"))
       (let ((bundle (make-instance 'image-directory
                                    :directory dir)))
         (finishes
          (upload-image-directory api-context
                                  bundle)))))))

(test the-same-image-isnt-uploaded-twice
  (with-fixture state ()
    (with-sdk-integration (api-context)
      (tmpdir:with-tmpdir (dir)
        (write-string-to-file "foobar" (path:catfile dir "one.png"))
        (write-string-to-file "foobar" (path:catfile dir "two.png"))
        (let ((put-file-counter 0))
         (cl-mock:with-mocks ()
           (cl-mock:if-called 'put-file
                              (lambda (api-context upload-url stream)
                                (incf put-file-counter)))
           (let ((bundle (make-instance 'image-directory
                                        :directory dir)))
             (finishes
               (upload-image-directory api-context
                                       bundle)))
           (is (eql 1 put-file-counter))))))))

(test the-same-image-isnt-uploaded-twice--more-complex
  (with-fixture state ()
    (with-sdk-integration (api-context)
      (tmpdir:with-tmpdir (dir)
        (write-string-to-file "foobar" (path:catfile dir "one.png"))
        (write-string-to-file "foobar" (path:catfile dir "two.png"))
        (write-string-to-file "foobar3" (path:catfile dir "three.png"))        
        (let ((put-file-counter 0))
         (cl-mock:with-mocks ()
           (cl-mock:if-called 'put-file
                              (lambda (api-context upload-url stream)
                                (incf put-file-counter)))
           (let ((bundle (make-instance 'image-directory
                                        :directory dir)))
             (finishes
               (upload-image-directory api-context
                                       bundle)))
           (is (eql 2 put-file-counter))))))))

(test simple-make-directory-run-happy-path
  (with-fixture state ()
    (with-sdk-integration (api-context)
     (tmpdir:with-tmpdir (dir)
       (write-string-to-file "foobar" (path:catfile dir "one.png"))
       (write-string-to-file "foobar2" (path:catfile dir "two.png"))
       (let ((bundle (make-instance 'image-directory
                                    :directory dir)))
         (finishes
           (make-directory-run api-context bundle
                               :repo (make-instance 'null-repo)
                               :channel "bleh")))))))

(test keyword-except-md5
  (is (eql 32 (length "82142ae81caba45bb76aa21fb6acf16d")))
  (is (equal "82142ae81caba45bb76aa21fb6acf16d" 
             (keyword-except-md5 "82142ae81caba45bb76aa21fb6acf16d")))
  ;; Notice that if we don't use a 32-byte string, then it gets
  ;; encoded differently.
  (is (equal "82142-AE-81-CABA-45-BB-76-AA-21-FB-6-ACF-16"
             (keyword-except-md5 "82142ae81caba45bb76aa21fb6acf16"))))
