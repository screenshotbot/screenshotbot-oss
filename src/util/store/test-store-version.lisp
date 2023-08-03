;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-store-version
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:*object-store*
                #:with-test-store)
  (:import-from #:util/store-version
                #:store-version
                #:version-subsystem
                #:*store-version*)
  (:import-from #:bknr.datastore
                #:store-directory)
  (:import-from #:bknr.datastore
                #:*store*)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:bknr.datastore
                #:*store*)
  (:import-from #:bknr.datastore
                #:mp-store)
  (:import-from #:bknr.datastore
                #:close-store)
  (:import-from #:bknr.datastore
                #:snapshot)
  (:import-from #:bknr.datastore
                #:*store*)
  (:import-from #:bknr.datastore
                #:store-subsystems)
  (:import-from #:util/store/store-version
                #:*snapshot-store-version*))
(in-package :util/store/test-store-version)


(util/fiveam:def-suite)

(def-easy-macro with-store (dir &fn fn)
  (when (boundp 'bknr.datastore:*store*)
    (error "Don't run this test in a live program with an existing store"))

  (let ((*store* nil))
    (unwind-protect
         (fn)
      (close-store))))


(def-fixture state ()
  (tmpdir:with-tmpdir (dir)
    (labels ((open-store ()
             (make-instance 'mp-store
                            :directory dir
                            :subsystems (list
                                         (make-instance 'version-subsystem))))
             (version-file ()
               (ensure-directories-exist
                (path:catfile dir "current/version-subsystem-snapshot")))
             (read-version ()
               (cond
                 ((path:-e (version-file))
                          (parse-integer (uiop:read-file-string (version-file))))
                 (t
                  :does-not-exist)))
             (get-subsystem ()
               (loop for subsystem in (store-subsystems *store*)
                     if (typep subsystem 'version-subsystem)
                       return subsystem))
             (write-version (ver)
               (with-open-file (s (version-file)
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
                 (format s "~a" ver))))
     (&body))))

(test simple-open ()
  (with-fixture state ()
    (with-store (dir)
      (open-store)
      (is (eql :does-not-exist
               (read-version)))
      (bknr.datastore:snapshot)
      (is (eql *store-version*
               (read-version)))
      (is (eql *store-version*
               *snapshot-store-version*)))))

(test open-existing-store ()
  (with-fixture state ()
    (open-store)
    (bknr.datastore:snapshot)
    (close-store)
    (write-version 2)
    (with-store (dir)
      (open-store)
      (is (eql 2 (read-version)))
      (is (eql 2 *snapshot-store-version*)))))

(test snapshot-updates-store-version
  (with-fixture state ()
    (with-store (dir)
      (open-store)
      (is (eql :does-not-exist (read-version)))
      (let ((*snapshot-store-version* 199990))
        (snapshot))
      (is (eql 199990 (read-version))))))
