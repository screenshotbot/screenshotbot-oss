;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :util/tests/test-store
    (:use #:cl
          #:fiveam
          #:alexandria)
    (:import-from #:util/store
                  #:def-store-local
                  #:find-any-refs
                  #:register-ref
                  #:with-test-store
                #:parse-timetag
                #:*object-store*
                #:object-store)
  (:import-from #:local-time
                #:timestamp=)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.datastore
                #:all-store-objects)
  (:import-from #:bknr.indices
                #:hash-index))
(in-package :util/tests/test-store)


(util/fiveam:def-suite)

(test object-store
  (tmpdir:with-tmpdir (tmp)
    (let ((*object-store* (namestring (path:catfile tmp "dummy/"))))
      (is (pathnamep (object-store)))
      (is (path:-d (object-store)))))
  (tmpdir:with-tmpdir (tmp)
    (let ((*object-store* (namestring (path:catfile tmp "dummy"))))
      (is (pathnamep (object-store)))
      (is (path:-d (object-store)))))

  #+nil ;; bad test that modifies global state
  (let ((*object-store* "~/zoidberg/bar/"))
    (is (equal #P "~/zoidberg/bar/" (object-store)))
    (is (path:-d (object-store)))))


#+lispworks
(test parse-timetag
  (is
   (timestamp=
    (local-time:parse-timestring "2021-10-09T06:00:00")
    (parse-timetag "20211009T060000"))))

(defclass dummy-class (store-object)
  ((name :initarg :name
         :index-type hash-index
         :index-initargs (:test #'equal)
         :index-reader dummy-class-for-name))
  (:metaclass persistent-class))

(test with-test-store
  (with-test-store ()
    (make-instance 'dummy-class)
    (make-instance 'dummy-class :name "bleh")
    (is (eql 2 (length (all-store-objects))))
    (is (eql 1 (length (dummy-class-for-name "bleh")))))
  (is (eql 0 (length (all-store-objects))))
  (is (eql 0 (length (dummy-class-for-name "bleh"))))
  (with-test-store ()
    (is (eql 0 (length (all-store-objects)))))
    (is (eql 0 (length (dummy-class-for-name "bleh")))))


(defclass xyz (store-object)
  ()
  (:metaclass persistent-class))

(defclass abc (store-object)
  ((ref :initarg :ref))
  (:metaclass persistent-class))

(def-fixture refs ()
  (with-test-store ()
    (&body)))

(test create-object-and-reference
  (with-test-store ()
    (let* ((xyz (make-instance 'xyz))
           (xyz-2 (make-instance 'xyz))
           (abc (make-instance 'abc :ref xyz)))
      (is (equal (list xyz abc) (find-any-refs (list xyz)))))
    (let* ((xyz (make-instance 'xyz))
           (abc (make-instance 'abc :ref (list xyz))))
      (is (equal (list xyz abc) (find-any-refs (list xyz)))))
    (let* ((xyz (make-instance 'xyz))
           (abc (make-instance 'abc :ref (make-array 1 :initial-contents (list xyz)))))
      (is (equal (list xyz abc) (find-any-refs (list xyz)))))))

(test transitive-refs
  (with-test-store ()
    (let* ((xyz (make-instance 'xyz))
           (abc-1 (make-instance 'abc :ref xyz))
           (abc (make-instance 'abc :ref abc-1)))
      (is (equal (list xyz abc-1 abc) (find-any-refs (list xyz))))))
  (with-test-store ()
    (let* ((xyz (make-instance 'xyz))
           (abc-1 (make-instance 'abc :ref xyz))
           (abc (make-instance 'abc :ref abc-1)))
      (is (equal (list xyz abc-1 abc) (find-any-refs (list abc-1 xyz)))))))

(test nested-in-list-refs
  (with-test-store ()
    (let* ((xyz (make-instance 'xyz))
           (abc-1 (make-instance 'abc :ref xyz))
           (abc (make-instance 'abc :ref (list 1 2 abc-1))))
      (is (equal (list xyz abc-1 abc) (find-any-refs (list xyz))))))
  (with-test-store ()
    (let* ((xyz (make-instance 'xyz))
           (abc-1 (make-instance 'abc :ref xyz))
           (abc (make-instance 'abc :ref (list xyz 2 abc-1))))
      (is (equal (list xyz abc-1 abc) (find-any-refs (list xyz))))))
  (with-test-store ()
    (let* ((xyz (make-instance 'xyz))
           (abc-1 (make-instance 'abc :ref xyz))
           (abc (make-instance 'abc :ref (list abc-1 2 xyz))))
      (is (equal (list xyz abc-1 abc) (find-any-refs (list xyz)))))))

(def-store-local *store-local* (make-hash-table)
  "A test cache")


(test store-local
  (let (one two)
    (with-test-store ()
      (setf one *store-local*)
      (is (eql one *store-local*)))
    (with-test-store ()
      (setf two *store-local*)
      (is (not (eql one two))))))

(test setf-store-local
  (let (one two)
    (with-test-store ()
      (setf *store-local* 'foo)
      (is (eql *store-local* 'foo)))
    (with-test-store ()
      (is (not (eql 'foo *store-local*)))
      (setf *store-local* 'bar)
      (is (eql 'bar *store-local*)))))

(test store-local-with-a-store
  (signals unbound-variable *store-local*))
