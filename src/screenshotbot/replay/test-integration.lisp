;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/test-integration
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/replay/integration
                #:with-sdk-flags
                #:all-screenshots
                #:process-results
                #:make-replay-run-context
                #:replay-job-from-snapshot
                #:run-in-parallel
                #:schedule-replay-job
                #:uploaded-snapshot
                #:remove-base-url
                #:get-local-addr)
  (:import-from #:screenshotbot/replay/core
                #:asset
                #:load-url-into
                #:snapshot-request
                #:snapshot)
  (:import-from #:screenshotbot/replay/browser-config
                #:browser-config)
  (:import-from #:screenshotbot/sdk/run-context
                #:main-branch)
  (:import-from #:screenshotbot/webdriver/impl
                #:make-driver
                #:call-with-webdriver)
  (:import-from #:screenshotbot/webdriver/screenshot
                #:full-page-screenshot)
  (:import-from #:screenshotbot/replay/replay-acceptor
                #:call-with-hosted-snapshot)
  (:import-from #:util/store
                #:*object-store*
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:prepare-singleton-company
                #:company)
  (:import-from #:screenshotbot/user-api
                #:user)
  (:import-from #:util/random-port
                #:random-port)
  (:import-from #:screenshotbot/server
                #:acceptor)
  (:import-from #:screenshotbot/api/recorder-run
                #:*synchronous-promotion*)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:screenshotbot/model/image
                #:with-local-image
                #:image
                #:image-blob)
  (:import-from #:screenshotbot/installation
                #:multi-org-feature
                #:installation
                #:*installation*)
  (:import-from #:screenshotbot/task-integration-api
                #:send-task)
  (:import-from #:screenshotbot/promote-api
                #:maybe-send-tasks)
  (:import-from #:screenshotbot/replay/proxy
                #:ensure-proxy
                #:*replay-proxy*
                #:*proxy-port*)
  (:import-from #:screenshotbot/replay/integration
                #:write-full-page-screenshot-from-handle
                #:fetch-full-page-screenshot-handle)
  (:import-from #:hunchentoot
                #:single-threaded-taskmaster)
  (:import-from #:util/testing
                #:with-global-binding)
  (:import-from #:screenshotbot/replay/services
                #:selenium-server
                #:call-with-selenium-server)
  (:import-from #:fiveam-matchers/core
                #:is-not
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains
                #:has-item)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:util/request
                #:*engine*)
  (:import-from #:util/hunchentoot-engine
                #:hunchentoot-engine)
  (:local-nicknames (#:a #:alexandria)
                    (#:integration #:screenshotbot/replay/integration)
                    (#:run-builder #:screenshotbot/replay/run-builder)))
(in-package :screenshotbot/replay/test-integration)


(util/fiveam:def-suite)

(test script-name
  (is (equal "/foo" (remove-base-url "https://google.com/foo")))
  (is (equal "/foo?bar=car" (remove-base-url "https://google.com/foo?bar=car"))))

(defclass fake-driver ()
  ())

(defmethod full-page-screenshot ((driver fake-driver) file)
  (error "this method should not be directly called"))

(defmethod fetch-full-page-screenshot-handle ((driver fake-driver) proxy)
  (values "123" (md5:md5sum-string "foobar")))

(defmethod write-full-page-screenshot-from-handle ((driver fake-driver)
                                                   proxy
                                                   oid file)
  (with-open-file (stream file :direction :output)
    (write-string "foobar" stream)))

(defclass test-installation (multi-org-feature
                             installation)
  ())

(defmacro with-test-globals (&body body)
  `(let ((auto-restart:*global-enable-auto-retries-p* nil))
     (tmpdir:with-tmpdir (tmp-store-dir)
       (with-global-binding ((*installation* (make-instance 'test-installation :plugins nil))
                             (*object-store* (namestring tmp-store-dir))
                             (*synchronous-promotion* t))
         ,@body))))

(defmacro with-proxy (&body body)
  `(let ((*proxy-port* (util/random-port:random-port))
         (*replay-proxy* nil))
     (unwind-protect
          (progn ,@body)
       (when *replay-proxy*
         (hunchentoot:stop *replay-proxy*)))))

(def-fixture state (&key host)
  (with-installation ()
   (with-global-binding ((lparallel:*kernel* nil))
     (with-test-store (:globally t)
       (with-test-globals
         (tmpdir:with-tmpdir (tmpdir)
           (cl-mock:with-mocks ()
             (with-open-file (dummy (path:catfile tmpdir "foo.html") :direction :output)
               (write-string "foo" dummy))
             (let* ((assets (list (make-instance 'asset
                                                 :url "https://www.google.com"
                                                 :status 200
                                                 :response-headers nil
                                                 :file "/foo.html")))
                    (snapshot (make-instance 'snapshot :tmpdir tmpdir
                                                       :root-urls (list
                                                                    "https://www.google.com" )
                                                       :assets assets))
                    (company (make-instance 'company))
                    (user (make-user
                           :companies (list company)))
                    (run (make-instance 'integration:run
                                        :company company
                                        :user user
                                        :host host
                                        :channel "test-channel"
                                        :browser-configs
                                        (list
                                         (make-instance 'browser-config
                                                        :name "firefox"
                                                        :type "firefox"))))
                    (fake-driver (make-instance 'fake-driver)))
               (cl-mock:if-called 'call-with-selenium-server
                                  (lambda (self fn &key type)
                                    (funcall fn
                                             (make-instance 'selenium-server
                                                            :host "foo.bar"
                                                            :squid-proxy "foo.bar:3128"
                                                            :port 9093
                                                            :type nil))))
               (cl-mock:if-called 'get-local-addr
                                  (lambda (&rest args)
                                    "de.ad.be.ef"))

               (cl-mock:if-called 'call-with-webdriver
                                  (lambda (fn &rest args)
                                    (let ((webdriver-client::*session*
                                            (make-instance 'webdriver-client::session
                                                           :id "dummy-test-session")))
                                      (funcall fn fake-driver))))
               (cl-mock:if-called '(setf webdriver-client:url)
                                  (lambda (url)
                                    nil))
               (cl-mock:if-called 'call-with-hosted-snapshot
                                  (lambda (company snapshot fn &rest args)
                                    (funcall fn "http://fake-url/")))

               (cl-mock:if-called 'ensure-proxy
                                  (lambda (selenium-service)
                                    "http://fakehost:5003"))
               (&body)))))))))

(test replay-job-from-snapshot
  (with-fixture state ()
    (is (typep
         (replay-job-from-snapshot
          :snapshot snapshot
          :urls (list
                 (cons "/" "https://www.google.com"))
          :run run
          :tmpdir tmpdir)
         'all-screenshots))))

(test process-results
  (with-installation (:globally t)
    (let ((engine (make-instance 'hunchentoot-engine
                                   :acceptor (make-instance 'acceptor)))
          (auto-restart:*global-enable-auto-retries-p* nil))
     (with-fixture state (:host "localhost:9098")
       (unwind-protect
            (let ((all-screenshots (make-instance 'all-screenshots
                                                  :company company)))
              (let ((pathname #.(asdf:system-relative-pathname
                                 :screenshotbot
                                 "fixture/rose.png")))
                (run-builder:record-screenshot
                 all-screenshots
                 :title "rose"
                 :md5 (md5:md5sum-file pathname)
                 :fetch (lambda (dest)
                          (fad:copy-file pathname dest))))
              (finishes
                (process-results
                 run
                 all-screenshots
                 :engine engine))))))))

(test if-old-image-is-rewritten-on-disk-we-still-dont-reupload
  (with-installation (:globally t)
    (let ((engine (make-instance 'hunchentoot-engine
                                 :acceptor (make-instance 'acceptor)))
          (auto-restart:*global-enable-auto-retries-p* nil))
      (with-fixture state (:host "localhost:9098")
        (unwind-protect
             (let ((all-screenshots (make-instance 'all-screenshots
                                                   :company company)))
               (let ((pathname #.(asdf:system-relative-pathname
                                  :screenshotbot
                                  "fixture/rose.png")))
                 (run-builder:record-screenshot
                  all-screenshots
                  :title "rose"
                  :md5 (md5:md5sum-file pathname)
                  :fetch (lambda (dest)
                           (fad:copy-file pathname dest))))
               (finishes
                 (process-results
                  run
                  all-screenshots
                  :engine engine))
               (let ((objs (bknr.datastore:store-objects-with-class 'image)))
                 (is (eql 1 (length objs))))
               (loop for im in (bknr.datastore:store-objects-with-class 'image)
                     do
                        (with-local-image (pathname im)
                          (fad:copy-file
                           #.(asdf:system-relative-pathname
                              :screenshotbot
                              "fixture/rose.webp")
                           pathname
                           :overwrite t)))

               (process-results
                run
                all-screenshots
                :engine engine)

               (is (eql 1 (length (bknr.datastore:store-objects-with-class 'image))))))))))

(def-fixture state-2 ()
  (with-installation ()
    (with-global-binding ((lparallel:*kernel* nil))
     (with-test-store ()
       (tmpdir:with-tmpdir (tmpdir)
         (cl-mock:with-mocks ()
           (cl-mock:if-called 'get-local-addr
                              (lambda (&rest args)
                                "9.9.9.9"))
           (cl-mock:if-called 'hunchentoot:start
                              (lambda (acceptor) (declare (ignore acceptor))))
           (let* ((company (make-instance 'company))
                  (*default-render-acceptor* nil)
                  (asset (make-instance 'asset
                                        :url "https://foo.com"
                                        :file "foo"))
                  (snapshot (make-instance 'snapshot
                                           :assets (list asset)
                                           :root-urls (list "https://foo.com")))
                  (run (make-instance 'integration:run
                                      :company company
                                      :browser-configs
                                      (list (make-instance 'browser-config
                                                           :type "chrome")))))
             (&body))))))))

(test replay-job-from-snapshot-2
  (with-fixture state-2 ()
    (cl-mock:if-called 'call-with-webdriver
                        (lambda (&rest args)
                          (declare (ignore args))))
    (replay-job-from-snapshot
     :run run
     :snapshot snapshot
     :urls (list "https://www.google.com")
     :tmpdir tmpdir)
    (pass)))

(defclass concurrent-plan (screenshotbot/plan::plan)
  ((concurrency :initarg :concurrency
                :reader concurrency)))

(defmethod integration:replay-concurrency (company (plan concurrent-plan))
  (concurrency plan))

(defun test-urls (n)
  (loop for i below n
        collect (cons (format nil "/~a" i)
                      (format nil "https://example.com/~a" i))))

(defun call-with-parallel-state (fn &key (concurrency 3) (on-batch (lambda ())))
  "Calls FN with a run, a selenium-server and a function that returns the
URLs that RUN-BATCH was called with so far. RUN-BATCH is mocked to
record its URLs and then call ON-BATCH.

This is a function and not a fixture, because a fixture expands into
the test body, which fiveam compiles when the test runs, and that
overflows the stack of the test thread."
  (with-fixture state-2 ()
    (let ((processed nil)
          (lock (bt:make-lock))
          (screenshotbot/plan::*plan* (make-instance 'concurrent-plan
                                                     :concurrency concurrency))
          (selenium-server (make-instance 'screenshotbot/replay/services::selenium-server
                                          :host "fake-selenium"
                                          :port 4444)))
      (cl-mock:if-called 'integration::run-batch
                         (lambda (&key urls &allow-other-keys)
                           (bt:with-lock-held (lock)
                             (setf processed (append urls processed)))
                           (funcall on-batch)))
      (funcall fn run selenium-server
               (lambda ()
                 (bt:with-lock-held (lock)
                   processed))))))

(test run-in-parallel-runs-every-url-once
  (let ((threads nil)
        (lock (bt:make-lock)))
    (call-with-parallel-state
     (lambda (run selenium-server processed)
       (run-in-parallel (test-urls 50) :run run :selenium-server selenium-server)
       (is (equal (sort (mapcar #'car (test-urls 50)) #'string<)
                  (sort (mapcar #'car (funcall processed)) #'string<))))
     :on-batch (lambda ()
                 (bt:with-lock-held (lock)
                   (pushnew (bt:current-thread) threads))))
    (is (eql 3 (length threads)))))

(test run-in-parallel-with-a-concurrency-of-1
  (call-with-parallel-state
   (lambda (run selenium-server processed)
     (run-in-parallel (test-urls 25) :run run :selenium-server selenium-server)
     (is (eql 25 (length (funcall processed)))))
   :concurrency 1))

(define-condition test-batch-error (error)
  ())

(test run-in-parallel-signals-a-failure-in-a-worker
  (let ((main-thread (bt:current-thread)))
    (call-with-parallel-state
     (lambda (run selenium-server processed)
       (handler-case
           (progn
             (run-in-parallel (test-urls 90) :run run :selenium-server selenium-server)
             (fail "expected a nursery-error"))
         (nursery:nursery-error (e)
           (let ((failures (nursery:nursery-error-failures e)))
             ;; A worker that starts after the other one failed stops
             ;; at its first CHECKPOINT, so there might be only one
             ;; failure.
             (is (<= 1 (length failures) 2))
             (is (every (lambda (failure)
                          (typep (nursery:failure-condition failure) 'test-batch-error))
                        failures)))))
       ;; Each thread has three batches of 10. The failure cancels the
       ;; nursery, so the main thread stops after its first batch.
       (is (<= (length (funcall processed)) 30)))
     :on-batch (lambda ()
                 (if (eql main-thread (bt:current-thread))
                     ;; Give the workers time to fail
                     (sleep 0.2)
                     (error 'test-batch-error))))))

(test run-in-parallel-safe-interrupt-stops-the-workers
  (let ((main-thread (bt:current-thread))
        (interruptedp nil))
    (call-with-parallel-state
     (lambda (run selenium-server processed)
       (handler-case
           (run-in-parallel (test-urls 90) :run run :selenium-server selenium-server)
         (util/threading:safe-interrupt ()
           (setf interruptedp t)))
       (is-true interruptedp)
       ;; Each thread has three batches of 10. Without the cancel, the
       ;; workers would run all six of theirs.
       (is (<= (length (funcall processed)) 30)))
     :on-batch (lambda ()
                 (if (eql main-thread (bt:current-thread))
                     (signal 'util/threading:safe-interrupt)
                     (sleep 0.2))))))

(test run-in-parallel-propagates-replay-logs
  (let ((seen nil)
        (lock (bt:make-lock))
        (logs (make-string-output-stream)))
    (call-with-parallel-state
     (lambda (run selenium-server processed)
       (declare (ignore processed))
       (let ((screenshotbot/replay/core:*replay-logs* logs))
         (run-in-parallel (test-urls 30) :run run :selenium-server selenium-server)))
     :on-batch (lambda ()
                 (bt:with-lock-held (lock)
                   (push screenshotbot/replay/core:*replay-logs* seen))))
    (is (eql 3 (length seen)))
    (is (every (lambda (stream) (eql logs stream)) seen))))

(def-fixture schedule-replay-job-mocks ()
  (let (used-snapshot)
    (cl-mock:if-called 'replay-job-from-snapshot
                       (lambda (&key snapshot &allow-other-keys)
                         (setf used-snapshot snapshot)
                         :fake-results))
    (cl-mock:if-called 'process-results
                       (lambda (run results &key &allow-other-keys)
                         (declare (ignore run))
                         results))
    (&body)))

(test schedule-replay-job-renders-uploaded-snapshot-without-recrawling
  (with-fixture state-2 ()
    (with-fixture schedule-replay-job-mocks ()
      (let ((run (make-instance 'integration:run
                                :company company
                                :request (make-instance 'snapshot-request
                                                        :snapshot snapshot
                                                        :channel-name "foo"
                                                        :browser-configs
                                                        (list (make-instance 'browser-config
                                                                             :type "chrome")))
                                :urls (list (cons "/" "https://foo.com"))
                                :browser-configs
                                (list (make-instance 'browser-config
                                                     :type "chrome")))))
        (cl-mock:if-called 'load-url-into
                           (lambda (&rest args)
                             (declare (ignore args))
                             (error "Should not re-crawl an uploaded snapshot")))
        (schedule-replay-job run)
        (is (eql snapshot used-snapshot))))))

(test schedule-replay-job-still-crawls-sitemap-style-runs
  (with-fixture state-2 ()
    (with-fixture schedule-replay-job-mocks ()
      (let ((run (make-instance 'integration:run
                                :company company
                                :urls (list (cons "/" "https://foo.com"))
                                :browser-configs
                                (list (make-instance 'browser-config
                                                     :type "chrome"))))
            (crawled-urls))
        (cl-mock:if-called 'load-url-into
                           (lambda (context snapshot url tmpdir &rest args)
                             (declare (ignore context tmpdir args))
                             (push url crawled-urls)
                             snapshot))
        (schedule-replay-job run)
        (is (equal (list "https://foo.com") crawled-urls))
        (is (typep used-snapshot 'snapshot))
        (is (not (eql snapshot used-snapshot)))))))


(def-fixture fake-acceptor (&rest options)
  ;; We're using a hunchentoot server here since it's the most common
  ;; use case. It also correctly works on both IPv6 and IPv4.
  (let* ((port (random-port))
         (acceptor (apply #'make-instance 'hunchentoot:acceptor :port
                          port
                          options)))
    (unwind-protect
         (progn
           (hunchentoot:start acceptor)
           (&body))
      (hunchentoot:stop acceptor))))

(test get-local-addr
  (with-fixture fake-acceptor ()
    (is (equal "127.0.0.1"
               (get-local-addr "127.0.0.1" port)))))

#+lispworks ;; currently breaking badly on SBCL and CCL
(test get-local-addr-on-ipv6
  (with-fixture fake-acceptor (:address "::1")
    (is (equal "::1"
               (get-local-addr "::1" port)))))

(test exclusions-on-sitemap
  (with-fixture state ()
    (cl-mock:if-called 'integration::parse-sitemap
                       (lambda (url)
                         (list
                          "https://example.com/google"
                          "https://example.com/facebook")))
    (let ((run (make-instance 'integration:run
                              :sitemap "https://www.google.com/sitemap.xml"
                              :exclusions (list ".*face.*"))))
      (assert-that (mapcar #'car (integration::urls run))
                   (is-not (has-item "/facebook"))
                   (has-item "/google")))))

(test with-sdk-flags-happy-path
  (let (result)
    (with-sdk-flags (:flags `((:metadata . "foobar")))
      (setf result :pass))
    (is (eql :pass result))))

(test with-sdk-flags-unknown-flag
  (let (result)
    (with-sdk-flags (:flags `((:unknown-flag . "foobar")
                              (:metadata . "bleh")))
      (is (equal "bleh" screenshotbot/sdk/flags:*metadata*))
      (setf result :pass))
    (is (eql :pass result))))

(test make-replay-run-context-uses-main-branch-from-request
  (let ((run (make-instance 'integration:run :channel "foo")))
    (is (equal "main"
               (main-branch
                (make-replay-run-context
                 run
                 (make-instance 'snapshot-request
                                :main-branch "main")))))))

(test make-replay-run-context-defaults-main-branch-to-master
  (let ((run (make-instance 'integration:run :channel "foo")))
    (is (equal "master"
               (main-branch
                (make-replay-run-context run nil))))
    (is (equal "master"
               (main-branch
                (make-replay-run-context
                 run
                 (make-instance 'snapshot-request)))))))

(test make-replay-run-context-uses-work-branch-from-request
  (let ((run (make-instance 'integration:run :channel "foo")))
    (is (equal "main"
               (screenshotbot/sdk/run-context:work-branch
                (make-replay-run-context
                 run
                 (make-instance 'snapshot-request
                                :work-branch "main")))))))
