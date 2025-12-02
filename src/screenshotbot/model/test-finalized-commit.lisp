;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-finalized-commit
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/finalized-commit
                #:finalized-commit
                #:finalized-commit-company
                #:finalized-commit-hash
                #:commit-finalized-p
                #:find-finalized-commit
                #:find-or-create-finalized-commit))
(in-package :screenshotbot/model/test-finalized-commit)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((company (make-instance 'company)))
     (&body))))

(test simple-test
  (with-fixture state ()
    (is (eql nil (commit-finalized-p company "foo")))
    (make-instance 'finalized-commit
                   :company company
                   :commit "foo")
    (is (eql t (commit-finalized-p company "foo")))))

(test restricted-to-company
  (with-fixture state ()
    (let ((company-2 (make-instance 'company)))
      (make-instance 'finalized-commit
                     :company company
                     :commit "foo")
      (is (eql t (commit-finalized-p company "foo"))))))

(test find-finalized-commit-returns-nil-when-not-found
  (with-fixture state ()
    (is (eql nil (find-finalized-commit company "nonexistent")))))

(test find-finalized-commit-returns-object-when-found
  (with-fixture state ()
    (let ((fc (make-instance 'finalized-commit
                             :company company
                             :commit "abc123")))
      (is (eql fc (find-finalized-commit company "abc123"))))))

(test find-finalized-commit-restricted-to-company
  (with-fixture state ()
    (let ((company-2 (make-instance 'company)))
      (let ((fc1 (make-instance 'finalized-commit
                                :company company
                                :commit "shared-commit"))
            (fc2 (make-instance 'finalized-commit
                                :company company-2
                                :commit "shared-commit")))
        (is (eql fc1 (find-finalized-commit company "shared-commit")))
        (is (eql fc2 (find-finalized-commit company-2 "shared-commit")))
        (is (not (eql fc1 fc2)))))))

(test find-or-create-finalized-commit-creates-when-not-found
  (with-fixture state ()
    (is (eql nil (find-finalized-commit company "new-commit")))
    (let ((fc (find-or-create-finalized-commit company "new-commit")))
      (is (not (null fc)))
      (is (typep fc 'finalized-commit))
      (is (eql company (finalized-commit-company fc)))
      (is (equal "new-commit" (finalized-commit-hash fc))))))

(test find-or-create-finalized-commit-returns-existing
  (with-fixture state ()
    (let ((fc1 (make-instance 'finalized-commit
                              :company company
                              :commit "existing")))
      (let ((fc2 (find-or-create-finalized-commit company "existing")))
        (is (eql fc1 fc2))
        (is (eql 1 (length (bknr.datastore:class-instances 'finalized-commit))))))))

(test find-or-create-finalized-commit-respects-company-isolation
  (with-fixture state ()
    (let ((company-2 (make-instance 'company)))
      (let ((fc1 (find-or-create-finalized-commit company "commit-sha"))
            (fc2 (find-or-create-finalized-commit company-2 "commit-sha")))
        (is (not (eql fc1 fc2)))
        (is (eql company (finalized-commit-company fc1)))
        (is (eql company-2 (finalized-commit-company fc2)))
        (is (eql 2 (length (bknr.datastore:class-instances 'finalized-commit))))))))
