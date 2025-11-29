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
  (:import-from #:fiveam-matchers/errors
                #:signals-error-matching
                #:error-with-string-matching)
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
                #:server-unavailable
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
                #:run-context-to-dto
                #:put-run-with-run-context
                #:empty-run-error
                #:find-existing-images
                #:get-relative-path
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
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/api/recorder-run
                #:warmup-image-caches
                #:*synchronous-promotion*)
  (:import-from #:screenshotbot/api/core
                #:*wrap-internal-errors*)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-branch
                #:recorder-run-merge-base
                #:recorder-run)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-commit)
  (:import-from #:screenshotbot/sdk/request
                #:%request)
  (:import-from #:screenshotbot/sdk/run-context
                #:invalid-pull-request)
  (:import-from #:screenshotbot/sdk/env
                #:make-env-reader)
  (:local-nicknames (#:a #:alexandria)
                    (#:api-key #:core/api/model/api-key)
                    (#:dto #:screenshotbot/api/model)
                    (#:flags #:screenshotbot/sdk/flags)
                    (#:run-context #:screenshotbot/sdk/run-context)))
(in-package :screenshotbot/sdk/test-sdk)

(util/fiveam:def-suite)


(def-fixture state ()
  (let ((*synchronous-promotion* t)
        (*wrap-internal-errors* nil))
    (cl-mock:with-mocks ()
      (if-called 'warmup-image-caches
                 (lambda (run)
                   (declare (ignore run))))
      (let ((auto-restart:*global-enable-auto-retries-p* nil))
        (util:copying (flags:*pull-request*
                       flags:*override-commit-hash*
                       flags:*main-branch*)
          (&body))))))

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
      (is (equal "http://foo" (run-context:pull-request-url
                               (make-instance 'run-context:flags-run-context
                                              :env (make-env-reader)))))))
  (is (eql 40 (length "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd")))
  (with-fixture state ()
    (with-env ((:circle-sha1 "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd")
               (:circle-pull-request "http://foo"))
      (parse-environment)
      (is (equal "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd" flags:*override-commit-hash*))))
  (with-fixture state ()
    (with-env ((:circle-sha1 "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd")
               (:circle-pull-request ""))
      (parse-environment)
      (is (equal "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd" flags:*override-commit-hash*)))))

(test parse-override-commit-hash-with-bitrise
    (with-fixture state ()
    (with-env ((:bitrise-git-commit "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd")
               (:bitriseio-pull-request-repository-url "https://bitbucket.org/tdrhq/fast-example"))
      (parse-environment)
      (is (equal nil (run-context:pull-request-url
                  (make-instance 'run-context:flags-run-context
                                 :env (make-env-reader)))))
      (is (equal "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd" flags:*override-commit-hash*))))

  (with-fixture state ()
    (with-env ((:bitrise-git-commit "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd")
               (:bitrise-pull-request "1")
               (:bitriseio-pull-request-repository-url "https://bitbucket.org/tdrhq/fast-example"))
      (parse-environment)

      (is (equal "https://bitbucket.org/tdrhq/fast-example/pull-requests/1"
                 (run-context:pull-request-url
                  (make-instance 'run-context:flags-run-context
                                 :env (make-env-reader)))))

      (is (equal "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcd" flags:*override-commit-hash*)))))

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
                                                  :main-branch "main"
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

(test empty-run-error-suggests-recursive-when-not-provided
  (let ((err (make-condition 'empty-run-error :recursivep nil)))
    (let ((message (with-output-to-string (s)
                     (format s "~A" err))))
      (is (str:containsp "--recursive" message))
      (is (str:containsp "Perhaps you wanted to use the --recursive flag?" message)))))

(test empty-run-error-does-not-suggest-recursive-when-already-provided
  (let ((err (make-condition 'empty-run-error :recursivep t)))
    (let ((message (with-output-to-string (s)
                     (format s "~A" err))))
      (is (not (str:containsp "--recursive" message)))
      (is (not (str:containsp "Perhaps you wanted to use the --recursive flag?" message)))
      (is (str:containsp "No screenshots were detected" message)))))

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
       (list "touch" "-d" "3 days ago" (namestring p)))
      (signals not-recent-file-warning
        (warn-if-not-recent-file s)))))

(test warn-if-not-recent-file-should-handle-nil-write-date
  (dotimes (i 10)
   (uiop:with-temporary-file (:pathname p :type "png"
                              :direction :output
                              :element-type 'flexi-streams:octet)
     (with-open-file (stream p :direction :output
                               :if-exists :supersede)
       (format stream "hello"))
     ;; I can't repro T1632 locally sadly
     (finishes
       (warn-if-not-recent-file p)))))

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

(def-easy-macro with-upload-image-directory (&binding api-context &binding images &fn fn)
  (with-sdk-integration (api-context)
    (tmpdir:with-tmpdir (dir)
      (write-string-to-file "foobar" (path:catfile dir "one.png"))
      (write-string-to-file "foobar" (path:catfile dir "two.png"))
      (write-string-to-file "foobar3" (path:catfile dir "three.png"))        
      (cl-mock:with-mocks ()
        (let ((bundle (make-instance 'image-directory
                                     :directory dir)))
          (let ((images
                  (upload-image-directory api-context
                                          bundle)))
            (fn api-context images)))))))

(test make-run-integration-test-1
  (with-fixture state ()
    (with-upload-image-directory (api-context images)
      (make-run api-context images
                :repo (make-instance 'null-repo)
                :channel "blah"))))

(defun the-only-run ()
  (car (class-instances 'recorder-run)))

(test make-run-integration-test-propagates-the-right-branch
  (with-fixture state ()
    (with-upload-image-directory (api-context images)
      (make-run api-context images
                :repo (make-instance 'null-repo)
                :branch "develop"
                :channel "blah")
      (is (equal "develop" (recorder-run-branch (the-only-run)))))))

(test make-run-integration-with-a-release-branch-regex
  (with-fixture state ()
    (with-upload-image-directory (api-context images)
      (put-run-with-run-context
       api-context
       (make-instance 'run-context:run-context
                      :channel "blah"
                      :main-branch "master"
                      :work-branch "release-2"
                      :release-branch-regex "release.*")
       images)
      (is (equal "release-2" (recorder-run-branch (the-only-run)))))))

(test make-run-integration-test-with-commit
  "Possible not required, especially if we remove the :commit parameter"
  (with-fixture state ()
    (with-upload-image-directory (api-context images)
      (make-run api-context images
                :repo (make-instance 'null-repo)
                :channel "blah"
                :commit "abcd"
                :merge-base "cbad"
                :branch-hash "dabc")
      (let ((run (the-only-run)))
        (is (equal "abcd" (recorder-run-commit run)))
        (is (equal "cbad" (recorder-run-merge-base run)))))))

(test keyword-except-md5
  (is (eql 32 (length "82142ae81caba45bb76aa21fb6acf16d")))
  (is (equal "82142ae81caba45bb76aa21fb6acf16d" 
             (keyword-except-md5 "82142ae81caba45bb76aa21fb6acf16d")))
  ;; Notice that if we don't use a 32-byte string, then it gets
  ;; encoded differently.
  (is (equal "82142-AE-81-CABA-45-BB-76-AA-21-FB-6-ACF-16"
             (keyword-except-md5 "82142ae81caba45bb76aa21fb6acf16"))))

(test adds-pixel-tolerance
  (is (eql 2 (dto:compare-pixel-tolerance
              (run-context-to-dto
               (make-instance 'run-context:run-context
                              :pixel-tolerance 2)
               nil #| screenshots |#)))))
