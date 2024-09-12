;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/api/test-send-tasks
  (:use #:cl
        #:alexandria
        #:bknr.datastore
        #:screenshotbot/model/recorder-run
        #:screenshotbot/model/user
        #:screenshotbot/user-api
        #:screenshotbot/api/promote
        #:screenshotbot/model/screenshot
        #:screenshotbot/promote-api
        #:screenshotbot/model/api-key
        #:screenshotbot/model/channel
        #:screenshotbot/model/company
        #:fiveam)
  (:import-from #:screenshotbot/github/task-integration
                #:*create-issue-fn*
                #:github-create-issue)
  (:import-from #:screenshotbot/tasks/common
                #:noop-task-integration
                #:get-enabled-task-integrations)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/model/screenshot-map
                #:make-screenshot-map)
  (:import-from #:screenshotbot/model/image
                #:make-image))

(util/fiveam:def-suite)

(defclass test-company (company)
  ()
  (:metaclass persistent-class))


(defmethod get-enabled-task-integrations ((company test-company) channel)
  (list
   (make-instance 'noop-task-integration
                  :company company)))

(def-fixture state ()
  (with-test-store ()
   (with-fake-request ()
     (auth:with-sessions ()
       (let ((promoter (make-instance 'master-promoter)))
         (with-test-user (:company company
                          :company-name "foobar enterprises"
                          :company-class 'test-company
                          :user user
                          :api-key api-key)
           (let* ((channel (make-instance 'channel :name "dfdfdf"
                                           ;; give a repo, just in case we
                                           ;; have a bug and we're actually
                                           ;; hitting github
                                           :github-repo "https://github.com/tdrhq/screenshotbot-example"
                                           :branch "master"))
                  (run1 (make-recorder-run
                         :channel channel
                         :company company
                         :commit-hash "car"
                         :cleanp t
                         :trunkp t))
                  (screenshot (make-instance 'screenshot
                                             :run nil
                                             :name "foo"))
                  (run2 (make-recorder-run
                         :channel channel
                         :commit-hash "car2"
                         :cleanp t
                         :screenshots (list screenshot)
                         :previous-run run1
                         :trunkp t
                         :company company)))
             (is (eql 'github-create-issue *create-issue-fn*))
             (let ((*create-issue-fn* (lambda (repo title body)
                                        "http://foo/1")))
               (&body)))))))))

(test happy-path
  (with-fixture state ()
    (is-false (company-reports company))
    (maybe-send-tasks promoter run2)
    (is-true (company-reports company))
    #+nil
    (is (equal "http://foo/1"
               (github-task (car (company-reports company)))))))

(test no-screenshots-no-task
  (with-fixture state ()
    (with-transaction ()
      (setf (run-screenshot-map run1)
            (make-screenshot-map channel nil))
      (setf (run-screenshot-map run2)
            (make-screenshot-map channel nil)))
    (maybe-send-tasks promoter run2)
    (is-false (company-reports company))))

(test happy-path-for-first-task
  (with-fixture state ()
    (maybe-send-tasks promoter run1)))
