;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-store
  (:use :cl)
  (:import-from #:bknr.datastore
                #:*store*
                #:all-store-objects
                #:deftransaction
                #:delete-object
                #:persistent-class
                #:store-object
                #:truncate-log)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:fiveam-matchers/core
                #:assert-that
                #:error-with-string-matching
                #:signals-error-matching)
  (:import-from #:it.bese.fiveam
                #:def-fixture
                #:fail
                #:finishes
                #:is
                #:is-false
                #:is-true
                #:pass
                #:signals
                #:test)
  (:import-from #:local-time
                #:timestamp=)
  (:import-from #:util/store/permissive-persistent-class
                #:permissive-persistent-class)
  (:import-from #:util/store/store
                #:object-neighbors
                #:*ensure-directories-cache*
                #:*object-store*
                #:*snapshot-hooks*
                #:def-store-local
                #:dispatch-snapshot-hooks
                #:fast-ensure-directories-exist
                #:find-any-refs
                #:location-for-oid
                #:object-store
                #:parse-timetag
                #:safe-mp-store
                #:verify-old-class
                #:with-snapshot-lock
                #:with-test-store)
  (:import-from #:fiveam-matchers/lists
                #:contains-in-any-order))
(in-package :util/store/test-store)


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

(defun fix-for-windows (x)
  (cond
   ((uiop:os-windows-p)
    (str:replace-all "/" "\\" x))
   (t
    x)))

(test location-for-oid--ensure-directory-is-writeable
  (with-test-store ()
    (with-open-file (file (location-for-oid  #P"foo-bar/" (mongoid:oid))
                          :direction :output)
      (write-string "hello" file))
    (pass)))

(test location-for-oid/expected-pathname
  (with-test-store ()
    (let ((pathname (location-for-oid #P "foo-bar/" #(#xab #xbc #x01 #x23
                                                      #xab #xbc #x01 #x23
                                                      #xab #xbc #x01 #x23))))
      (is (str:ends-with-p
           (fix-for-windows "foo-bar/ab/bc/0123abbc0123abbc0123")
           (namestring pathname)))
      ;; Ensure absolute
      (is (not (str:starts-with-p "foo-bar" (namestring pathname)))))))

(test location-for-oid/expected-relative-path
  (with-test-store ()
    (let ((pathname (nth-value
                     1
                     (location-for-oid #P "foo-bar/" #(#xab #xbc #x01 #x23
                                                       #xab #xbc #x01 #x23
                                                       #xab #xbc #x01 #x23)))))
      (is (string=
           (fix-for-windows "foo-bar/ab/bc/0123abbc0123abbc0123")
           (namestring pathname))))))

(test location-for-oid/with-suffix
  (with-test-store ()
    (let ((pathname (location-for-oid #P "foo-bar/" #(#xab #xbc #x01 #x23
                                                      #xab #xbc #x01 #x23
                                                      #xab #xbc #x01 #x23)
                                      :suffix "metadata")))
      (is (str:ends-with-p
           (fix-for-windows "foo-bar/ab/bc/0123abbc0123abbc0123-metadata")
           (namestring pathname))))))

(test cant-access-deleted-object-slots
  "This is checking some changes we have in bknr.datastore. In particular,
 making sure our slot-value-using-class is correctly wired. (See
 indexed-class.lisp or D5784)"
  (with-test-store ()
    (let ((obj (make-instance 'dummy-class :name "foo")))
      (is (equal "foo" (slot-value obj 'name)))
      (delete-object obj)
      (handler-case
          (progn
           (slot-value obj 'name)
           (fail "expected error"))
        (error (e)
          (is (str:containsp "Can not get slot NAME of destroyed"
                             (format nil "~a" e))))))))

(test dispatch-snapshot-hooks
  (with-test-store ()
   (let (ret)
     (flet ((hook (store dir)
              (push dir ret)))
       (let ((*snapshot-hooks* (list #'hook)))
         (dispatch-snapshot-hooks :foo))
       (is (equal (list :foo) ret))))))

(test safe-snapshot-happy-path
  (with-test-store ()
    (let ((*snapshot-hooks* nil))
      (util:safe-snapshot "dummy"))))

(test |body get's called :/|
  (let ((calledp nil))
    (with-test-store ()
      (setf calledp t))
    (is-true calledp)))

(test snapshot-lock
  (with-test-store ()
    (is
     (eql :foo
      (with-snapshot-lock (*store*)
        :foo)))))

(test can-compare-deleted-objects
  (with-test-store ()
    (let ((obj-1 (make-instance 'dummy-class))
          (obj-2 (make-instance 'dummy-class)))
      (flet ((checks ()
               (is (eql :less (fset:compare obj-1 obj-2)))
               (is (eql :equal (fset:compare obj-1 obj-1)))
               (is (eql :greater (fset:compare obj-2 obj-1)))))
        (checks)
        (delete-object obj-1)
        (checks)))))

(defvar *state* 0)

(deftransaction dummy-transaction (arg)
  (incf *state*))

(test fast-ensure-directories-exist
  (tmpdir:with-tmpdir (dir)
    (let ((path (path:catdir dir "foo/bar/car")))
      (is (equalp path (fast-ensure-directories-exist path)))
      (is (path:-d (path:catdir dir "foo/bar/")))
      (is-true (fset:@ *ensure-directories-cache* (pathname-directory path)))
      (is (equalp path (fast-ensure-directories-exist path)))

      ;; Make sure we're not hitting the disk.. so let's delete the
      ;; directory and try again.
      #+lispworks
      (progn
        (lw:delete-directory (path:catdir dir "foo/bar/"))
        (is (equalp path (fast-ensure-directories-exist path)))
        ;; We shouldn't have actually hit the disk to do this
        (is-false (path:-d (path:catdir dir "foo/bar/")))))))

(defclass dummy-class-2 ()
  ((a) (b) (c)))


(test verify-old-class-with-persistent-class
  (finishes
   (verify-old-class 'dummy-class-2
                     '((a) (b) (c))
                     'persistent-class))
  (signals-error-matching ()
   (verify-old-class 'dummy-class-2
                     '((a) (b) (cl-user::c))
                     'persistent-class)
   (error-with-string-matching
    "missing slots: (C)"))
  (finishes
    (verify-old-class 'dummy-class-2
                      '((a) (cl-user::b) (c))
                      'permissive-persistent-class))
  (signals-error-matching ()
    (verify-old-class 'dummy-class-2
                      '((cl-user::b) (c))
                      'permissive-persistent-class)
    (error-with-string-matching
     "missing slots: (A)")))


(test object-neighbors-for-list
  (assert-that
   (object-neighbors '(1 2 3))
   (contains-in-any-order
    1 2 3)))

(defclass some-object (store-object)
  ((a) (b) (c) (d) (e) (f) (g) (h))
  (:metaclass persistent-class))

(test benchmark-snapshots
  (with-test-store ()
    (loop for i from 0 to 10 ;; increase
          do (make-instance 'some-object))
    (progn ;; time or hcl:profile
     (bknr.datastore:snapshot))))
