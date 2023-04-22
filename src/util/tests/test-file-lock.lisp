;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util.test-file-locks
  (:use :cl
   :alexandria
        :fiveam)
  (:import-from
   :util/file-lock
   :file-lock
   :release-file-lock)
  (:import-from #:util/file-lock
                #:could-not-get-lock
                #:lock-not-held))
(in-package :util.test-file-locks)

(util/fiveam:def-suite)

(test simple-lock-unlock
  (tmpdir:with-tmpdir (dir)
    (let ((lock (make-instance 'file-lock :file (path:catfile dir "test.lock"))))
      (release-file-lock lock)
      (pass))))

(test second-unlock-errors
  (tmpdir:with-tmpdir (dir)
    (let ((lock (make-instance 'file-lock :file (path:catfile dir "test.lock"))))
      (release-file-lock lock)
      (signals lock-not-held
        (release-file-lock lock)))))

(test shared-lock-can-be-held-multiple-times
  (tmpdir:with-tmpdir (dir)
    (let ((file (path:catfile dir "test.lock")))
     (let ((lock (make-instance 'file-lock :file file
                                           :sharedp t)))
       (let ((lock-2 (make-instance 'file-lock :file file
                                               :sharedp t)))
         (finishes (release-file-lock lock-2)))
       (finishes (release-file-lock lock))))))

(test acquire-lock-after-unlocking
  (tmpdir:with-tmpdir (dir)
    (let ((file (path:catfile dir "test.lock")))
     (let ((lock (make-instance 'file-lock :file file)))
       (release-file-lock lock)
       (let ((lock-2 (make-instance 'file-lock :file file
                                               :timeout 5)))
         (release-file-lock lock-2)
         (pass))))))

(test write-lock-after-shared-lock
  (tmpdir:with-tmpdir (dir)
    (let ((file (path:catfile dir "test.lock")))
     (let ((lock (make-instance 'file-lock :file file
                                           :sharedp t)))
       (signals could-not-get-lock
         (make-instance 'file-lock :file file
                        :timeout -10))
       (finishes (release-file-lock lock))))))
