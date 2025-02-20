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
                #:base-indexed-object
                #:object-destroyed-p
                #:hash-index)
  (:import-from #:fiveam-matchers/core
                #:does-not
                #:assert-that
                #:error-with-string-matching
                #:signals-error-matching)
  (:import-from #:it.bese.fiveam
                #:with-fixture
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
                #:parse-timestring
                #:timestamp=)
  (:import-from #:util/store/permissive-persistent-class
                #:permissive-persistent-class)
  (:import-from #:util/store/store
                #:snapshot-timestamps-to-delete
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
                #:has-item
                #:contains-in-any-order)
  (:import-from #:util/benchmark
                #:def-benchmark)
  (:import-from #:alexandria
                #:curry
                #:rcurry)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:import-from #:fiveam-matchers/strings
                #:starts-with))
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
      (is-false (object-destroyed-p obj))
      (delete-object obj)
      (is (typep obj 'base-indexed-object))
      (is-true (object-destroyed-p obj))
      (block nil
        (handler-case
            (slot-value obj 'name)
          (error (e)
            (is (str:containsp "Can not get slot NAME of destroyed"
                               (format nil "~a" e)))
            (return nil)))
        (fail "expected error")))))

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

(def-fixture snapshots-to-delete ()
  ;; Real timestamps!
  (let* ((timestamps
          "2024-12-22T06:01:31.202776Z.tar.gz
2024-12-23T06:01:27.057467Z.tar.gz
2024-12-24T06:01:28.090583Z.tar.gz
2024-12-25T06:01:29.742115Z.tar.gz
2024-12-26T06:01:31.607890Z.tar.gz
2024-12-27T06:01:37.463320Z.tar.gz
2024-12-28T06:01:27.491526Z.tar.gz
2024-12-29T06:01:33.181207Z.tar.gz
2024-12-30T06:01:24.178870Z.tar.gz
2024-12-31T06:01:27.035301Z.tar.gz
2025-01-01T06:01:32.059641Z.tar.gz
2025-01-02T06:01:31.765585Z.tar.gz
2025-01-03T06:01:29.687138Z.tar.gz
2025-01-04T06:01:40.347807Z.tar.gz
2025-01-05T06:01:33.716786Z.tar.gz
2025-01-06T06:01:29.211524Z.tar.gz
2025-01-07T06:01:35.581537Z.tar.gz
2025-01-08T06:01:33.514377Z.tar.gz
2025-01-09T06:01:31.519362Z.tar.gz
2025-01-10T06:01:24.333182Z.tar.gz
2025-01-11T06:01:21.292447Z.tar.gz
2025-01-12T06:01:37.254664Z.tar.gz
2025-01-13T06:01:32.775781Z.tar.gz
2025-01-14T06:01:34.168139Z.tar.gz
2025-01-15T06:01:35.893747Z.tar.gz
2025-01-16T06:01:38.791653Z.tar.gz
2025-01-17T06:01:33.023031Z.tar.gz
2025-01-18T06:01:36.142141Z.tar.gz
2025-01-19T06:01:31.520360Z.tar.gz
2025-01-20T06:01:35.939607Z.tar.gz
2025-01-21T06:01:36.385916Z.tar.gz
2025-01-22T06:01:48.918824Z.tar.gz
2025-01-23T06:01:40.217339Z.tar.gz
2025-01-24T06:01:35.115817Z.tar.gz
2025-01-25T06:01:40.610016Z.tar.gz
2025-01-26T06:01:35.515151Z.tar.gz
2025-01-26T18:07:42.179451Z.tar.gz
2025-01-26T20:29:23.857867Z.tar.gz
2025-01-27T06:01:35.691601Z.tar.gz
2025-01-28T06:01:45.479065Z.tar.gz
2025-01-29T06:01:38.980523Z.tar.gz
2025-01-30T06:01:40.951473Z.tar.gz
2025-01-31T06:01:42.953026Z.tar.gz
2025-02-01T06:01:41.071904Z.tar.gz
2025-02-02T06:01:39.273604Z.tar.gz
2025-02-03T06:01:36.975659Z.tar.gz
2025-02-04T06:01:32.387555Z.tar.gz
2025-02-05T06:01:39.869423Z.tar.gz
2025-02-06T06:01:38.406103Z.tar.gz
2025-02-07T06:01:39.696043Z.tar.gz
2025-02-08T06:01:32.506827Z.tar.gz
2025-02-09T06:01:44.899164Z.tar.gz
2025-02-10T06:01:36.662999Z.tar.gz
2025-02-11T06:01:34.943563Z.tar.gz
2025-02-12T06:01:44.472930Z.tar.gz
2025-02-13T06:01:33.937036Z.tar.gz
2025-02-14T06:01:40.742849Z.tar.gz
2025-02-15T06:01:40.056559Z.tar.gz
2025-02-16T06:01:35.962157Z.tar.gz
2025-02-17T06:01:36.954105Z.tar.gz
2025-02-18T06:01:42.249969Z.tar.gz
2025-02-19T06:01:36.439887Z.tar.gz")
         (timestamps (str:lines timestamps))
         (timestamps (mapcar
                      #'local-time:parse-timestring
                      (loop for ts in timestamps
                            collect (str:replace-all ".tar.gz" "" ts)))))
    (&body)))

(defun %snapshot-timestamps-to-delete (timestamps &rest args &key now &allow-other-keys)
  (mapcar (lambda (ts)
            (local-time:format-timestring nil ts :timezone local-time:+utc-zone+) )
          (apply #'snapshot-timestamps-to-delete timestamps :now (parse-timestring now) args)))

(test simple-snapshots-to-delete
  (with-fixture snapshots-to-delete ()
    (let ((result (%snapshot-timestamps-to-delete timestamps :now "2025-02-20T06:01:36.439887Z")))
      (assert-that result
                   (does-not (has-item "2025-02-16T06:01:35.962157Z"))))
    (let ((result (%snapshot-timestamps-to-delete timestamps :now "2027-02-20T06:01:36.439887Z")))
      (assert-that result
                   (has-item "2025-02-16T06:01:35.962157Z")))
    (let ((to-delete (%snapshot-timestamps-to-delete timestamps :now "2025-02-20T06:00:36.439887Z"
                                                                :keep-none 1
                                                                :keep-all 1)))
      (assert-that to-delete
                   (does-not (has-item "2025-02-19T06:01:36.439887Z"))
                   (has-item "2025-02-18T06:01:42.249969Z")
                   ;; Everything except the last is deleted
                   (has-length (1- (length timestamps)))))
    (let ((to-delete (%snapshot-timestamps-to-delete timestamps :now "2025-02-20T06:00:36.439887Z"
                                                                :keep-none 2
                                                                :keep-all 1)))
      (assert-that to-delete
                   (does-not (has-item "2025-02-19T06:01:36.439887Z"))
                   (does-not (has-item "2025-02-18T06:01:42.249969Z"))
                   ;; Everything except the last two deleted
                   (has-length (- (length timestamps) 2))))))

(test snapshots-to-delete-over-the-weeks
  (with-fixture snapshots-to-delete ()
    (let ((to-delete (%snapshot-timestamps-to-delete timestamps :now "2025-02-20T06:00:36.439887Z"
                                                                :keep-none 90
                                                                :keep-all 1)))
      (assert-that to-delete
                   (does-not (has-item "2025-02-19T06:01:36.439887Z"))
                   (described-as "the last timestamp in the week is not deleted"
                     (does-not (has-item "2025-02-18T06:01:42.249969Z")))
                   (has-item "2025-02-17T06:01:36.954105Z")
                   (has-item (starts-with "2025-01-15"))
                   (has-item (does-not (starts-with "2025-01-16")))
                   (has-item (starts-with "2025-01-14"))))))
