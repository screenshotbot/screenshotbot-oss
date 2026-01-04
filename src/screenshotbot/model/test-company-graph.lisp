;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-company-graph
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:multi-org-test-installation
                #:with-installation
                #:with-test-user)
  (:import-from #:screenshotbot/model/company-graph
                #:*lparallelp*
                #:save-images
                #:company-full-graph
                #:company-graph)
  (:import-from #:fiveam-matchers/lists
                #:has-item)
  (:import-from #:fiveam-matchers/core
                #:does-not
                #:is-not
                #:assert-that)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:util/store/object-id
                #:oid)
  (:import-from #:screenshotbot/model/constant-string
                #:constant-string)
  (:local-nicknames (#:roles #:auth/model/roles)))
(in-package :screenshotbot/model/test-company-graph)

(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*lparallelp* nil))
   (with-test-store ()
     (with-installation (:installation (make-instance 'multi-org-test-installation))
       (&body)))))

(test happy-path-graph
  (with-fixture state ()
    (with-test-user (:user user :company company)
      (assert-that (company-graph company)
                   (has-item user))
      (with-test-user (:user other-user :company company2 :company-name "other company")
        (assert-that (company-graph company)
                     (is-not (has-item other-user)))))))

(test full-graph-without-connecting-the-companies
  (with-fixture state ()
    (with-test-user (:user user :company company)
      (assert-that (company-full-graph company)
                   (has-item user))
      (with-test-user (:user other-user :company company2 :company-name "other company")
        (assert-that (company-full-graph company)
                     (is-not (has-item other-user)))))))

(defclass dummy-class (store-object)
  ((one :initarg :one)
   (two :initarg :two
        :reader two))
  (:metaclass persistent-class)
  (:documentation "Sometimes you might connect two objects with something like this."))

(test full-graph-finds-everything
  (with-fixture state ()
    (with-test-user (:user user :company company)
      (with-test-user (:user other-user :company company2 :company-name "other company")
        (make-instance 'dummy-class
                       :one user
                       :two other-user)
        (is (not (equal company2 company)))
        (assert-that (company-full-graph company)
                     (has-item other-user)
                     (has-item user)
                     (has-item company2)
                     (has-item company))))))

(test full-graph-doesnt-go-via-strings
  (with-fixture state ()
    (with-test-user (:user user :company company)
      (let ((str "abcde"))
       (with-test-user (:user other-user :company company2 :company-name "other company")
         (let ((obj1 (make-instance 'dummy-class
                                    :one company
                                    :two str))
               (obj2 (make-instance 'dummy-class
                                    :one company2
                                    :two str)))
           (is (eql (two obj1)
                    (two obj2))))
         (is (not (equal company2 company)))
         (assert-that (company-full-graph company)
                      (has-item company)
                      (does-not (has-item company2))))))))


(test full-graph-doesnt-go-via-constant-strings
  (with-fixture state ()
    (with-test-user (:user user :company company)
      (let ((str "abcde"))
       (with-test-user (:user other-user :company company2 :company-name "other company")
         (let ((obj1 (make-instance 'dummy-class
                                    :one company
                                    :two (constant-string str)))
               (obj2 (make-instance 'dummy-class
                                    :one company2
                                    :two (constant-string str))))
           (is (eql (two obj1)
                    (two obj2))))
         (is (not (equal company2 company)))
         (assert-that (company-full-graph company)
                      (has-item company)
                      (has-item (constant-string "abcde"))
                      (does-not (has-item company2))))))))


(test copies-images
  (with-fixture state ()
    (with-test-user (:company company)
      (let ((img (make-image :pathname (asdf:system-relative-pathname :screenshotbot "fixture/rose.png")
                             :oid #(1 2 3 4 1 2 3 4 1 2 3 4)
                             :company company)))
        (is (equal "010203040102030401020304" (oid img)))
        (tmpdir:with-tmpdir (dir)
          (finishes
            (save-images (company-full-graph company) :output dir))
          (is-true
           (path:-d (path:catdir dir "image-blobs/01/")))
          (is-true
           (path:-e (path:catfile dir "image-blobs/01/02/03040102030401020304"))))))))

(test test-reverse-graph-basic-structure
  ;; Tests that reverse-graph creates the correct inverse relationships
  ;; This ensures the graph structure is properly inverted for traversal
  (with-fixture state ()
    (with-test-user (:user user :company company)
      (let ((graph (screenshotbot/model/company-graph::reverse-graph :undirected t)))
        (is (hash-table-p graph))
        ;; Verify that the user-roles object connects user and company
        (let ((user-roles (find-if (lambda (obj)
                                     (and (typep obj 'roles::user-roles)
                                          (eq (roles:role-user obj) user)
                                          (eq (roles:role-company obj) company)))
                                   (bknr.datastore:store-objects-with-class 'roles::user-roles))))
          (assert-that (gethash user-roles graph)
                       (has-item company)
                       (has-item user)))))))

(test test-reverse-graph-ignores-atoms
  ;; Tests that reverse-graph properly excludes atomic values like strings and numbers
  ;; This prevents false connections through shared primitive values
  (with-fixture state ()
    (with-test-user (:user user :company company)
      (let ((obj1 (make-instance 'dummy-class
                                 :one company
                                 :two "shared-string"))
            (obj2 (make-instance 'dummy-class
                                 :one user
                                 :two 42)))
        (let ((graph (screenshotbot/model/company-graph::reverse-graph)))
          ;; Strings and numbers should not appear as keys in the reverse graph
          (is (null (gethash "shared-string" graph)))
          (is (null (gethash 42 graph))))))))


(test fset-map
  (with-fixture state ()
    (with-test-user (:user user :company company)
      (let* ((key1 user)
             (val1 company)
             (key2 "string-key")
             (val2 42)
             (test-map (fset:map (key1 val1) (key2 val2))))
        (let ((neighbors (screenshotbot/model/company-graph::object-neighbors-for-graph test-map)))
          (assert-that neighbors
                       (has-item key1)
                       (has-item val1)
                       (has-item key2)
                       (has-item val2))
          ;; Verify we get exactly 4 items (2 keys + 2 values)
          (is (= 4 (length neighbors))))))))



(test hash-table-object-neighbors-for-graph
  (with-fixture state ()
    (with-test-user (:user user :company company)
      (let ((test-hash (make-hash-table :test 'equal)))
        (setf (gethash user test-hash) company)
        (setf (gethash "key1" test-hash) "value1")
        (setf (gethash 42 test-hash) user)
        (let ((neighbors (screenshotbot/model/company-graph::object-neighbors-for-graph test-hash)))
          (assert-that neighbors
                       (has-item user)
                       (has-item company)
                       (has-item "key1")
                       (has-item "value1")
                       (has-item 42))
          ;; Verify we get exactly 6 items (3 keys + 3 values)
          (is (= 6 (length neighbors))))))))

