;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-archived-run
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/archived-run
                #:archived-run
                #:archived-run-commit
                #:archived-run-branch
                #:archived-run-author
                #:archived-run-cleanp
                #:archived-run-created-at
                #:archived-run-compare-threshold
                #:archived-run-compare-tolerance
                #:archived-run-channel
                #:archived-run-company
                #:archived-run-metadata
                #:archived-run-tags
                #:archived-run-screenshots
                #:save-archived-run
                #:load-archived-run
                #:oid)
  (:import-from #:screenshotbot/api/model
                #:metadata
                #:metadata-key
                #:metadata-value)
  (:import-from #:json-mop
                #:json-to-clos)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:assert-that)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-channel
                #:run-screenshot-map
                #:recorder-run-warnings
                #:recorder-run-branch
                #:recorder-run-tags)
  (:import-from #:screenshotbot/user-api
                #:%created-at
                #:channel
                #:user)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/screenshot-map
                #:screenshot-map)
  (:import-from #:screenshotbot/model/api-key
                #:api-key)
  (:import-from #:auth/viewer-context
                #:api-viewer-context)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/model/test-archived-run)

(util/fiveam:def-suite)

(test write-and-read-archived-run
  "Test writing an archived-run to JSON and reading it back"
  (let* ((run (make-instance 'archived-run
                             :commit-hash "abc123def456"
                             :branch "main"
                             :author "test-author"
                             :cleanp t
                             :created-at 1234567890
                             :compare-threshold 0.05
                             :compare-tolerance 10
                             :channel 12345
                             :company "test-company"))
         (json-output (with-output-to-string (out)
                        (json-mop:encode run out)))
         (read-run (json-to-clos json-output 'archived-run)))

    ;; Verify the data matches
    (is (equal "abc123def456" (archived-run-commit read-run)))
    (is (equal "main" (archived-run-branch read-run)))
    (is (equal "test-author" (archived-run-author read-run)))
    (is (equal 12345 (archived-run-channel read-run)))
    (is (equal "test-company" (archived-run-company read-run)))

    ;; Verify boolean is parsed correctly
    (is (equal t (archived-run-cleanp read-run)))
    (is (typep (archived-run-cleanp read-run) 'boolean))

    ;; Verify integers are parsed correctly
    (is (equal 1234567890 (archived-run-created-at read-run)))
    (is (typep (archived-run-created-at read-run) 'integer))

    ;; Verify numbers are parsed correctly
    (is (< (abs (- 0.05 (archived-run-compare-threshold read-run))) 0.001))
    (is (typep (archived-run-compare-threshold read-run) 'number))
    (is (equal 10 (archived-run-compare-tolerance read-run)))
    (is (typep (archived-run-compare-tolerance read-run) 'number))))

(test write-multiple-archived-runs
  "Test encoding multiple archived-runs to JSON"
  (let* ((run1 (make-instance 'archived-run
                              :commit-hash "commit1"
                              :branch "main"
                              :cleanp t))
         (run2 (make-instance 'archived-run
                              :commit-hash "commit2"
                              :branch "develop"
                              :cleanp nil))
         (json1 (with-output-to-string (out)
                  (json-mop:encode run1 out)))
         (json2 (with-output-to-string (out)
                  (json-mop:encode run2 out)))
         (read-run1 (json-to-clos json1 'archived-run))
         (read-run2 (json-to-clos json2 'archived-run)))

    ;; Verify the data
    (is (equal "commit1" (archived-run-commit read-run1)))
    (is (equal "commit2" (archived-run-commit read-run2)))

    ;; Verify booleans work with both true and false
    (is (equal t (archived-run-cleanp read-run1)))
    (is (typep (archived-run-cleanp read-run1) 'boolean))
    (is (equal nil (archived-run-cleanp read-run2)))
    (is (typep (archived-run-cleanp read-run2) 'boolean))))

(test archived-run-with-metadata
  "Test that archived-run properly serializes and deserializes metadata objects"
  (let* ((meta1 (make-instance 'metadata
                               :key "build-id"
                               :value "12345"))
         (meta2 (make-instance 'metadata
                               :key "environment"
                               :value "production"))
         (run (make-instance 'archived-run
                             :commit-hash "test-commit"
                             :branch "main"
                             :metadata (list meta1 meta2)))
         (json-output (with-output-to-string (out)
                        (json-mop:encode run out)))
         (read-run (json-to-clos json-output 'archived-run)))

    ;; Verify basic fields
    (is (equal "test-commit" (archived-run-commit read-run)))
    (is (equal "main" (archived-run-branch read-run)))

    ;; Verify metadata is a list
    (is (listp (archived-run-metadata read-run)))
    (is (= 2 (length (archived-run-metadata read-run))))

    ;; Verify first metadata object
    (let ((read-meta1 (first (archived-run-metadata read-run))))
      (is (typep read-meta1 'metadata))
      (is (equal "build-id" (metadata-key read-meta1)))
      (is (equal "12345" (metadata-value read-meta1))))

    ;; Verify second metadata object
    (let ((read-meta2 (second (archived-run-metadata read-run))))
      (is (typep read-meta2 'metadata))
      (is (equal "environment" (metadata-key read-meta2)))
      (is (equal "production" (metadata-value read-meta2))))))

(test save-and-load-archived-run
  "Test saving and loading an archived-run to/from disk"
  (with-test-store ()
   (uiop:with-temporary-file (:pathname temp-dir :type :directory)
     (let* ((run (make-instance 'archived-run
                                :oid "507f1f77bcf86cd799439011"
                                :commit-hash "abc123def456"
                                :branch "main"
                                :author "test-author"
                                :cleanp t
                                :created-at 1234567890
                                :compare-threshold 0.05
                                :compare-tolerance 10
                                :channel 12345
                                :company "test-company"))
            (oid-str (oid run :stringp t)))

       ;; Save the run
       (let ((file-path (save-archived-run run)))
         (assert-that
          (namestring file-path)
          (matches-regex ".*/50/7f/1f77bcf86cd799439011" ))
         (is (not (null file-path)))
         (is (probe-file file-path)))

       ;; Load the run back
       (let ((loaded-run (load-archived-run oid-str)))
         ;; Verify the data matches
         (is (equal "abc123def456" (archived-run-commit loaded-run)))
         (is (equal "main" (archived-run-branch loaded-run)))
         (is (equal "test-author" (archived-run-author loaded-run)))
         (is (equal 12345 (archived-run-channel loaded-run)))
         (is (equal "test-company" (archived-run-company loaded-run)))
         (is (equal t (archived-run-cleanp loaded-run)))
         (is (equal 1234567890 (archived-run-created-at loaded-run)))
         (is (< (abs (- 0.05 (archived-run-compare-threshold loaded-run))) 0.001))
         (is (equal 10 (archived-run-compare-tolerance loaded-run))))))))

(test non-existent-archived-run
  (with-test-store ()
    (is (eql nil
             (load-archived-run "507f1f77bcf86cd799439011")))))

(test archived-run-recorder-run-channel
  "Test that recorder-run-channel works with archived-run"
  (with-test-store ()
    (let* ((company (make-instance 'company))
           (channel (make-instance 'channel :company company))
           (channel-id (store-object-id channel))
           (run (make-instance 'archived-run
                               :channel channel-id
                               :company (store-object-id company))))
      ;; Verify that recorder-run-channel returns the channel object
      (is (eql channel (recorder-run-channel run))))))

(test archived-run-run-screenshot-map
  "Test that run-screenshot-map works with archived-run"
  (with-test-store ()
    (let* ((company (make-instance 'company))
           (channel (make-instance 'channel :company company))
           (screenshot-map (make-instance 'screenshot-map :channel channel))
           (screenshot-map-id (store-object-id screenshot-map))
           (run (make-instance 'archived-run
                               :screenshots screenshot-map-id
                               :channel (store-object-id channel)
                               :company (store-object-id company))))
      ;; Verify that run-screenshot-map returns the screenshot-map object
      (is (eql screenshot-map (run-screenshot-map run))))))

(test archived-run-recorder-run-warnings
  "Test that recorder-run-warnings returns nil for archived-run"
  (with-test-store ()
    (let* ((run (make-instance 'archived-run
                               :commit-hash "test-commit")))
      ;; Verify that recorder-run-warnings returns nil
      (is (eql nil (recorder-run-warnings run))))))

(test archived-run-recorder-run-branch-reader
  "Test that recorder-run-branch reader works with archived-run"
  (let* ((run (make-instance 'archived-run
                             :branch "feature-branch")))
    ;; Verify that recorder-run-branch returns the branch
    (is (equal "feature-branch" (recorder-run-branch run)))))

(test archived-run-created-at-reader
  "Test that %created-at reader works with archived-run"
  (let* ((timestamp 1234567890)
         (run (make-instance 'archived-run
                             :created-at timestamp)))
    ;; Verify that %created-at returns the timestamp
    (is (equal timestamp (%created-at run)))))

(test archived-run-recorder-run-tags-reader
  "Test that recorder-run-tags reader works with archived-run"
  (let* ((run (make-instance 'archived-run
                             :tags "v1.0.0")))
    ;; Verify that recorder-run-tags returns the tags
    (is (equal "v1.0.0" (recorder-run-tags run)))))

(test archived-run-can-view
  "Test that auth:can-view works with archived-run"
  (with-test-store ()
    (let* ((company (make-instance 'company))
           (channel (make-instance 'channel :company company))
           (user (make-instance 'user))
           (run (make-instance 'archived-run
                               :channel (store-object-id channel)
                               :company (store-object-id company))))
      ;; Give the user access to the company
      (roles:ensure-has-role company user 'roles:standard-member)
      ;; Verify that the user can view the run
      (is (equal t (auth:can-view run user))))))

(test archived-run-can-viewer-view
  "Test that auth:can-viewer-view works with archived-run"
  (with-test-store ()
    (let* ((company (make-instance 'company))
           (channel (make-instance 'channel :company company))
           (user (make-instance 'user))
           (api-key (make-instance 'api-key
                                   :user user
                                   :company company
                                   :permissions '(:full)))
           (run (make-instance 'archived-run
                               :channel (store-object-id channel)
                               :company (store-object-id company))))
      ;; Give the user access to the company
      (roles:ensure-has-role company user 'roles:standard-member)
      ;; Verify that the viewer can view the run
      (assert-that
       (auth:can-viewer-view
        (make-instance 'api-viewer-context
                       :user user
                       :api-key api-key)
        run)
       (is-equal-to t)))))
