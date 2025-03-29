;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-recorder-run
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/recorder-run
                #:%run-build-url
                #:run-build-url
                #:clean-up-old-shards
                #:find-shards
                #:shard
                #:unchanged-run-other-commit
                #:unchanged-run
                #:runs-for-channel
                #:delete-run
                #:runs-for-company
                #:recorder-run-author
                #:%author
                #:assert-no-loops
                #:runs-for-tag
                #:make-recorder-run
                #:pull-request-id
                #:transient-promotion-log
                #:promotion-log
                #:recorder-run)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:equal-to
                #:has-typep
                #:assert-that)
  (:import-from #:bknr.datastore
                #:class-instances
                #:with-transaction)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:fiveam-matchers/lists
                #:has-item
                #:contains)
  (:import-from #:screenshotbot/user-api
                #:user
                #:recorder-previous-run
                #:recorder-run-commit
                #:channel)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:screenshotbot/model/channel
                #:production-run-for)
  (:import-from #:screenshotbot/model/company
                #:find-or-create-channel
                #:company)
  (:import-from #:screenshotbot/model/api-key
                #:api-key)
  (:import-from #:auth/viewer-context
                #:api-viewer-context)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:import-from #:core/api/model/api-key
                #:cli-api-key)
  (:import-from #:screenshotbot/model/constant-string
                #:constant-string)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/model/test-recorder-run)

(util/fiveam:def-suite)

(def-fixture state (&key (api-key-roles :both))
  (dolist (api-key-roles
           (ecase api-key-roles
             (:both
              '(gk:enable gk:disable))
             (:disable
              '(gk:disable))
             (:enable
              '(gk:enable))))
    (with-test-store ()
      (gk:create :api-key-roles)
      (funcall api-key-roles :api-key-roles)
      (let* ((run (make-recorder-run))
             (company (make-instance 'company))
             (promotion-log (make-instance 'promotion-log)))
        (&body)))))


(test promotion-log-for-new
  (with-fixture state ()
    (assert-that (promotion-log run)
                 (has-typep 'transient-promotion-log))
    (is (pathnamep
         (blob-pathname (promotion-log run))))))

(test pull-request-id
  (with-fixture state ()
    (let ((run2 (make-recorder-run
                 :pull-request "https://foo/bar/20")))
      (is (eql nil (pull-request-id run)))
      (is (eql 20 (pull-request-id run2))))))

(test maintains-channel-runs
  (with-fixture state ()
    (let* ((channel (make-instance 'channel))
           (run (make-recorder-run
                 :commit-hash "bleh2"
                 :channel channel
                 :trunkp t)))
      (assert-that (production-run-for channel :commit "car")
                   (has-length 0))
      (assert-that (fset:convert 'list (runs-for-channel channel))
                   (contains run))
      (assert-that (production-run-for channel :commit "bleh2")
                   (is-equal-to run))
      (bknr.datastore:delete-object run)
      (assert-that (fset:convert 'list (runs-for-channel channel))
                   (has-length 0))
      (assert-that (production-run-for channel :commit "bleh2")
                   (has-length 0)))))

(test if-commit-is-not-present-try-override-commit
  (with-fixture state ()
    (let* ((channel (make-instance 'channel))
           (run (make-recorder-run
                 :override-commit-hash "bleh")))
      (is (equal "bleh" (recorder-run-commit run))))))

(test if-both-commit-and-override-commit-is-not-present
  "Then we return nil without failing"
  (with-fixture state ()
    (let* ((channel (make-instance 'channel))
           (run (make-recorder-run)))
      (is (equal nil (recorder-run-commit run))))))

(test finding-runs-by-tags
  (with-fixture state ()
    (let* ((company2 (make-instance 'company))
           (run1 (make-recorder-run
                     :company company
                     :tags (list "foo" "bar")))
           (run2 (make-recorder-run
                  :company company
                  :tags (list "bar")))
           (run3 (make-recorder-run
                  :company company))
           (run4 (make-recorder-run
                  :company company2
                  :tags (list "foo"))))
      (assert-that (runs-for-tag company "foo")
                   (contains run1))
      (assert-that (runs-for-tag company "bar")
                   (has-item run1)
                   (has-item run2)))))

(test assert-no-loops
  (with-fixture state ()
    (let ((run (make-recorder-run
                :previous-run (make-recorder-run
                               :previous-run (make-recorder-run
                                              :previous-run nil)))))
      (finishes (assert-no-loops run))
      (setf (recorder-previous-run (recorder-previous-run (recorder-previous-run run)))
            run)
      (signals simple-error
        (assert-no-loops run)))
    (let ((run (make-recorder-run
                :previous-run (make-recorder-run
                               :previous-run nil))))
      (finishes (assert-no-loops run)))))

(test unbound-slot-for-author
  (with-fixture state ()
    (let ((run (make-recorder-run)))
      (slot-makunbound run '%author)
      (is (eql nil (recorder-run-author run))))))


(test delete-run-happy-path
  (with-fixture state ()
    (let ((run (make-recorder-run :company company)))
      (assert-that (fset:convert 'list (runs-for-company company))
                   (contains run))
      (delete-run run)
      (assert-that (fset:convert 'list (runs-for-company company))
                   (contains))
      (finishes
        (delete-run run)))))


(test can-save-and-restore-unchanged-runs
  (tmpdir:with-tmpdir (dir)
    (with-test-store (:dir dir)
      (make-instance 'unchanged-run
                     :commit "foo"
                     :other-commit "bar"
                     :channel (find-or-create-channel (make-instance 'company) "bleh" ))
      (bknr.datastore:snapshot))
    (with-test-store (:dir dir)
      (is (equal "bar"
                 (unchanged-run-other-commit (car
                                              (class-instances 'unchanged-run))))))))


(test can-save-and-restore-recorder-runs
  (tmpdir:with-tmpdir (dir)
    (with-test-store (:dir dir)
      (make-recorder-run
       :commit-hash "foo"
       :channel (find-or-create-channel (make-instance 'company) "bleh"))
      (bknr.datastore:snapshot))
    (with-test-store (:dir dir)
      (is (equal "foo"
                 (recorder-run-commit (car
                                       (class-instances 'recorder-run))))))))

(test api-viewer-context-can-only-see-runs-in-the-company
  (with-fixture state ()
    (let* ((other-company (make-instance 'company))
           (user (make-instance 'user))
           (api-key (make-instance 'api-key
                                   :user user
                                   :permissions '(:full)
                                   :company other-company))
           (other-channel (make-instance 'channel
                                         :company other-company))
           (other-run (make-recorder-run
                       :channel other-channel
                       :company other-company))
           (channel (make-instance 'channel
                                         :company company))
           (run (make-recorder-run
                       :channel channel
                       :company company)))
      (roles:ensure-has-role company user 'roles:standard-member)
      (roles:ensure-has-role other-company user 'roles:standard-member)
      (assert-that
       (auth:can-viewer-view
        (make-instance
         'api-viewer-context
         :user user
         :api-key (make-instance 'api-key
                                 :user user
                                 :permissions '(:full)
                                 :company other-company))
        other-run)
       (described-as "Can view runs in the same company"
         (is-equal-to t)))
      (assert-that
       (auth:can-viewer-view
        (make-instance
         'api-viewer-context
         :user user
         :api-key api-key)
        run)
       (described-as "Cannot view runs in the other company"
         (is-equal-to nil))))))

(def-fixture can-viewer-view-fixture (&key (permissions '(:ci)))
  (let* ((channel (make-instance 'channel :company company))
           (user (make-instance 'user))
           (run (make-recorder-run
                 :channel channel
                 :company company))
           (api-key (make-instance 'api-key
                                   :permissions permissions
                                   :user user
                                   :company company)))
      (roles:ensure-has-role company user 'roles:standard-member)
    (&body)))

(test api-viewer-context-cant-view-run-if-only-ci-permissions
  (with-fixture state (:api-key-roles :enable)
    (with-fixture can-viewer-view-fixture ()
      (assert-that
       (auth:can-viewer-view
        (make-instance
         'api-viewer-context
         :user user
         :api-key api-key)
        run)
       (is-equal-to nil)))))

(test api-viewer-context-can-view-if-full-permissions
  (with-fixture state (:api-key-roles :enable)
    (with-fixture can-viewer-view-fixture (:permissions '(:full))
      (assert-that
       (auth:can-viewer-view
        (make-instance
         'api-viewer-context
         :user user
         :api-key api-key)
        run)
       (is-equal-to t)))))


(test api-viewer-context-can-view-if-gk-is-disabled
  (with-fixture state (:api-key-roles :disable)
    (with-fixture can-viewer-view-fixture ()
      (assert-that
       (auth:can-viewer-view
        (make-instance
         'api-viewer-context
         :user user
         :api-key api-key)
        run)
       (is-equal-to t)))))

(test api-viewer-context-can-view-runs-from-cli-api-key
  (with-fixture state (:api-key-roles :enable)
    (with-fixture can-viewer-view-fixture ()
      (let ((api-key (make-instance 'cli-api-key
                                    :user user
                                    :company company)))
        (assert-that
         (auth:can-viewer-view
          (make-instance
           'api-viewer-context
           :user user
           :api-key api-key)
          run)
         (is-equal-to t))))))

(test lookup-shards
  (with-fixture state ()
    (let* ((company-2 (make-instance 'company))
           (channel (make-instance 'channel
                                   :company company))
           (channel-2 (make-instance 'channel
                                     :company company-2)))
      (make-instance 'shard
                     :channel channel-2
                     :key "foo")
      (make-instance 'shard
                     :channel channel
                     :key "foo")
      (make-instance 'shard
                     :channel channel
                     :key "foo")
      (make-instance 'shard
                     :channel channel                     
                     :key "bar")
      (assert-that (find-shards channel "foo")
                   (has-length 2)))))

(test clean-up-old-shards
  (with-fixture state ()
   (let ((shard1 (make-instance 'shard
                                :ts 100))
         (shard2 (make-instance 'shard)))
     (clean-up-old-shards)
     (assert-that (class-instances 'shard)
                  (contains shard2)))))


(test build-url-for-recorder-run
  (with-fixture state ()
    (let* ((company (make-instance 'company))
           (channel (make-instance 'channel
                                   :company company))
           (run (make-recorder-run
                 :channel channel
                 :screenshots nil
                 :build-url "foobar")))
      (is (equal "foobar" (run-build-url run)))
      (is (typep (%run-build-url run) 'constant-string)))))
