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
