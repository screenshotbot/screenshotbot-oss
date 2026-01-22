;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(defpackage :screenshotbot/azure/test-request
  (:use #:cl
        #:fiveam)
  (:import-from #:util/request
                #:*engine*
                #:http-request-impl)
  (:import-from #:screenshotbot/azure/request
                #:azure-unauthorized-error
                #:azure-request
                #:azure)
  (:import-from #:screenshotbot/azure/promoter
                #:push-remote-check-impl)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:check))
(in-package :screenshotbot/azure/test-request)


(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
    ;; if you need a non-401-engine, still keep this around as a
    ;; default for these dtests.
    (with-test-store ()
      (let* ((*engine* (make-instance '401-engine))
             (company (make-instance 'company))
             (run (make-recorder-run :screenshots nil :company company)))
        (gk:create :azure-commit-status)
        (let ((azure (make-instance 'azure
                                    :token "dfd"
                                    :organization "testsbot"
                                    :project "fast-example")))
          (&body))))))

(defclass 401-engine ()
  ())

(defvar *request-callback* (lambda (url &key &allow-other-keys)
                             (values "" 401 nil)))

(defmethod http-request-impl ((self 401-engine)
                              url &rest args &key &allow-other-keys)
  (apply *request-callback* self args))

(test handles-401-more-gracefully
  (with-fixture state ()
    (signals azure-unauthorized-error
      (azure-request
       azure
       "foo/bar"
       :method :post))))

(test push-remote-check-impl-happy-path
  (let ((*request-callback* (lambda (url &key &allow-other-keys)
                    (values "{}" 200 nil))))
    (with-fixture state ()
      (finishes
       (push-remote-check-impl
        azure
        :run run
        :repo "testbot/fast-example"
        :check (make-instance 'check
                              :key "blahblah"
                              :sha "abcd"
                              :title "Foobarxs"
                              :details-url "https://example.com"
                              :status :accepted))))))

(test push-remote-check-impl-with-new-azure-commit-status
  (let ((*request-callback* (lambda (url &key &allow-other-keys)
                              (values "{}" 200 nil))))
    (with-fixture state ()
      (gk:allow :azure-commit-status company)
      (finishes
       (push-remote-check-impl
        azure
        :run run
        :repo "testbot/fast-example"
        :check (make-instance 'check
                              :key "blahblah"
                              :sha "abcd"
                              :title "Foobarxs"
                              :details-url "https://example.com"
                              :status :accepted))))))

