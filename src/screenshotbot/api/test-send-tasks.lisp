;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/api/test-send-tasks
  (:use #:cl
        #:alexandria
        #:bknr.datastore
        #:../model/recorder-run
        #:../model/user
        #:../user-api
        #:./promote
        #:../model/screenshot
        #:../promote-api
        #:../model/api-key
        #:../model/channel
        #:../model/company
        #:fiveam)
  (:import-from #:../github/task-integration
                #:*create-issue-fn*
                #:github-create-issue)
  (:import-from #:../tasks/common
                #:noop-task-integration
                #:get-enabled-task-integrations)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:../testing
                #:with-test-user))

(def-suite* :screenshotbot/api/test-send-tasks)

(defclass test-company (company)
  ()
  (:metaclass persistent-class))


(defmethod get-enabled-task-integrations ((company test-company) channel)
  (list
   (make-instance 'noop-task-integration
                  :company company)))

(def-fixture state ()
  (util:with-fake-request ()
    (auth:with-sessions ()
      (let ((promoter (make-instance 'master-promoter)))
       (with-test-user (:company company
                        :company-class 'test-company
                        :user user
                        :api-key api-key)
         (let* ((channel (make-instance 'channel :name "dfdfdf"
                                        ;; give a repo, just in case we
                                        ;; have a bug and we're actually
                                        ;; hitting github
                                                 :github-repo "https://github.com/tdrhq/screenshotbot-example"
                                                 :branch "master"))
                (run1 (make-instance 'recorder-run
                                      :channel channel
                                      :company company
                                      :commit-hash "car"
                                      :cleanp t
                                      :trunkp t))
                (screenshot (make-instance 'screenshot
                                            :run nil
                                            :name "foo"))
                (run2 (make-instance 'recorder-run
                                      :channel channel
                                      :commit-hash "car2"
                                      :cleanp t
                                      :screenshots (list screenshot)
                                      :previous-run run1
                                      :trunkp t
                                      :company company)))
           (is (eql 'github-create-issue *create-issue-fn*))
           (let ((*current-api-key* api-key)
                 (*create-issue-fn* (lambda (repo title body)
                                                     "http://foo/1")))
             (&body))))))))

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
      (setf (recorder-run-screenshots run1) nil)
      (setf (recorder-run-screenshots run2) nil))
    (maybe-send-tasks promoter run2)
    (is-false (company-reports company))))

(test happy-path-for-first-task
  (with-fixture state ()
    (maybe-send-tasks promoter run1)))
