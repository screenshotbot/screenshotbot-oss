(defpackage :util.test-file-locks
  (:use :cl
   :alexandria
        :fiveam)
  (:import-from
   :util
   :file-lock
   :release-file-lock))
(in-package :util.test-file-locks)


(test simple-lock-unlock
  (tmpdir:with-tmpdir (dir)
    (let ((lock (make-instance 'file-lock :file (path:catfile dir "test.lock"))))
      (release-file-lock lock)
      (pass))))
