;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/bitbucket/test-promoter
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/pro/bitbucket/promoter
                #:make-key
                #:send-build-status
                #:bitbucket-promoter)
  (:import-from #:screenshotbot/github/pull-request-promoter
                #:check
                #:make-task-args)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:cl-mock
                #:if-called)
  (:import-from #:screenshotbot/pro/bitbucket/settings
                #:get-access-token-from-refresh-token
                #:bitbucket-token)
  (:import-from #:screenshotbot/pro/bitbucket/audit-log
                #:audit-log-error-response
                #:audit-log-error
                #:bitbucket-audit-logs-for-company)
  (:import-from #:screenshotbot/pro/bitbucket/core
                #:bitbucket-error)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/bitbucket/test-promoter)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-installation ()
   (with-test-store ()
     (let* ((channel (make-instance 'channel :name "channel-0"))
            (company (make-instance 'company))
            (bitbucket-token (make-instance 'bitbucket-token
                                            :refresh-token "fake-refresh-token"
                                            :company company))
            (check (make-instance 'check
                                  :title "No screenshots changed"
                                  :status :success)))
       (cl-mock:with-mocks ()
         (if-called 'bitbucket-settings-for-company
                    (lambda (c)
                      (assert (eql c company))
                      (list bitbucket-token)))
         (if-called 'util/request:http-request
                    (lambda (&rest args)
                      (error "Unimplemented http-request mock for args ~a" args)))
         (&body))))))

(test make-task-args-happy-path
  (with-fixture state ()
    (let* ((run (make-instance 'recorder-run :channel channel))
           (promoter (make-instance 'bitbucket-promoter)))
      (let ((result (make-task-args promoter
                              run
                              "foobar"
                              check)))
        (is (equal "SUCCESSFUL" (a:assoc-value result :state)))))))

(test make-task-args-override-commit-hash
  (with-fixture state ()
    (let ((run (make-instance 'recorder-run
                               :channel channel
                               :override-commit-hash "foobar"
                               :commit-hash "zoidberg"))
          (promoter (make-instance 'bitbucket-promoter)))
      (let ((result (make-task-args promoter
                              run
                              "foobar"
                              check)))
        (is (equal "foobar" (a:assoc-value result :commit)))))))

(test make-task-args-no-commit-hash
  (with-fixture state ()
    (let ((run (make-instance 'recorder-run
                               :channel channel
                               :commit-hash "zoidberg"))
          (promoter (make-instance 'bitbucket-promoter)))
      (let ((result (make-task-args promoter
                              run
                              "foobar"
                              check)))
        (is (equal "zoidberg" (a:assoc-value result :commit)))))))

(test send-build-status-makes-audit-log
  (with-fixture state ()
    (if-called 'get-access-token-from-refresh-token
                (lambda (company token)
                  (assert (equal "fake-refresh-token" token))
                  "fake-access-token"))
    (if-called 'util/request:http-request
                (lambda (url &key &allow-other-keys)
                  (assert (str:ends-with-p "statuses/build/" url))
                  (values
                   (make-string-input-stream
                    (json:encode-json-to-string
                     `((:key . "screenshotbot--blehbleh"))))
                   204))
                :at-start t)
    (send-build-status company
                       `((:commit . "abcd")
                         (:full-name . "tdrhq/fast-example")))
    (is (eql 1 (length (bitbucket-audit-logs-for-company company))))
    (let ((audit-log (car (bitbucket-audit-logs-for-company company))))
      (is (eql nil (audit-log-error audit-log)))
      (is (eql nil (audit-log-error-response audit-log))))))

(test send-build-status-has-error
  (with-fixture state ()
    (if-called 'get-access-token-from-refresh-token
                (lambda (company token)
                  (assert (equal "fake-refresh-token" token))
                  "fake-access-token"))
    (if-called 'util/request:http-request
                (lambda (url &key &allow-other-keys)
                  (assert (str:ends-with-p "statuses/build/" url))
                  (values
                   (make-string-input-stream
                    "{
  \"errors\": [
    {
      \"exceptionName\": \"<string>\",
      \"message\": \"<string>\",
      \"context\": \"<string>\"
    }
  ]
}")
                   401))
                :at-start t)
    (send-build-status company
                       `((:commit . "abcd")
                         (:full-name . "tdrhq/fast-example")))
    (is (eql 1 (length (bitbucket-audit-logs-for-company company))))
    (let ((audit-log (car (bitbucket-audit-logs-for-company company))))
     (is (not (str:emptyp (audit-log-error audit-log)))))))

(test make-key
  (is (equal "screenshotbot--xx-yyy-android"
             (make-key "xx-yyy-android")))
  (let ((name "12345678-12345678-123-android"))
    (is (equal (ironclad:byte-array-to-hex-string
                (md5:md5sum-string (format nil "screenshotbot--~a" name)))
               (make-key name)))
    (is (<= (length (make-key name)) 40))))
