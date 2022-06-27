;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/test-integration
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/replay/integration
                #:all-screenshots
                #:process-results
                #:replay-job-from-snapshot
                #:remove-base-url
                #:get-local-addr)
  (:import-from #:screenshotbot/replay/core
                #:asset
                #:snapshot)
  (:import-from #:screenshotbot/replay/browser-config
                #:browser-config)
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
                #:prepare-acceptor-plugins
                #:acceptor)
  (:import-from #:screenshotbot/api/recorder-run
                #:*synchronous-promotion*)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:screenshotbot/model/image
                #:image-filesystem-pathname
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
                #:with-local-acceptor)
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

(defmethod fetch-full-page-screenshot-handle ((driver fake-driver))
  (values "123" (md5:md5sum-string "foobar")))

(defmethod write-full-page-screenshot-from-handle ((driver fake-driver)
                                                   oid file)
  (with-open-file (stream file :direction :output)
    (write-string "foobar" stream)))

(defclass test-installation (multi-org-feature
                             installation)
  ())

(defmacro with-test-globals (&body body)
  `(let ((old-installation *installation*))
     (tmpdir:with-tmpdir (tmp-store-dir)
       (progn
         (setf *object-store* (namestring tmp-store-dir))
         (setf *synchronous-promotion* t)
         (setf *installation* (make-instance 'test-installation :plugins nil))
         (unwind-protect
              (progn
                ,@body)
           (setf *synchronous-promotion* nil)
           (setf *installation* old-installation)
           (makunbound '*object-store*))))))

(defmacro with-proxy (&body body)
  `(let ((*proxy-port* (util/random-port:random-port))
         (*replay-proxy* nil))
     (unwind-protect
          (progn ,@body)
       (when *replay-proxy*
         (hunchentoot:stop *replay-proxy*)))))

(def-fixture state (&key host)
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
                                                    :root-files (list
                                                                 "/foo.html" )
                                                    :assets assets))
                 (company (make-instance 'company))
                 (user (make-instance 'user
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
                                (lambda ()
                                  "http://fakehost:5003"))
            (&body)))))))

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
  (with-local-acceptor (host
                        :prepare-acceptor-callback (lambda (acceptor)
                                                     (prepare-acceptor-plugins acceptor)))
      ('acceptor)
   (with-fixture state (:host host)
     (unwind-protect
          (let ((all-screenshots (make-instance 'all-screenshots
                                                 :company company)))
            (let ((pathname (asdf:system-relative-pathname
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
               )))))))

(test if-old-image-is-rewritten-on-disk-we-still-dont-reupload
  (let ((port (random-port)))
    (with-fixture state (:host (format nil "http://localhost:~a" port))
      (let ((acceptor (make-instance 'acceptor
                                      :port port)))
        (prepare-acceptor-plugins acceptor)
        (hunchentoot:start acceptor)

        (unwind-protect
             (let ((all-screenshots (make-instance 'all-screenshots
                                                    :company company)))
               (let ((pathname (asdf:system-relative-pathname
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
                  all-screenshots))
               (let ((objs (bknr.datastore:store-objects-with-class 'image)))
                (is (eql 1 (length objs))))
               (loop for im in (bknr.datastore:store-objects-with-class 'image)
                     do
                        (fad:copy-file
                         (asdf:system-relative-pathname
                          :screenshotbot
                          "fixture/rose.webp")
                         (image-filesystem-pathname im)
                         :overwrite t))

               (process-results
                run
                all-screenshots)

               (is (eql 1 (length (bknr.datastore:store-objects-with-class 'image)))))


          (hunchentoot:stop acceptor))))))

(def-fixture state-2 ()
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
                                      :file "foo"))
               (snapshot (make-instance 'snapshot
                                         :assets (list asset)
                                         :root-files (list "foo")))
               (run (make-instance 'integration:run
                                    :company company
                                    :browser-configs
                                    (list (make-instance 'browser-config
                                                          :type "chrome")))))
          (&body))))))

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
