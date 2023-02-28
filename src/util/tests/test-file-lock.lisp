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
