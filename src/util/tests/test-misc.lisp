;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-misc
  (:use #:cl
        #:fiveam)
  (:import-from #:util/misc
                #:safe-ensure-directories-exist
                #:or-setf)
  (:import-from #:tmpdir
                #:with-tmpdir)
  (:import-from #:fiveam-matchers
                #:is-equal-to
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-misc)


(util/fiveam:def-suite)

(test or-setf-happy-path
  (let ((var nil))
    (is
     (equal "foo"
            (or-setf
             var
             "foo"
             :thread-safe t)))
    (is (equal "foo" var))
    (is
     (equal "foo"
      (or-setf
       var
       "car")))
    (is (equal "foo" var))))


(test safe-ensure-directories-exist
  (with-tmpdir (dir)
    (dotimes (i 10)
     (let ((errors))
       (let ((threads
               (loop for i from 0 to 50
                     collect
                     (bt:make-thread
                      (lambda ()
                        (handler-case
                            (safe-ensure-directories-exist
                             (path:catfile dir "a/b/c/d/e/f/g/hi/j/k/l/m/n/o/p/q/r/s/t/u/v/w/x/y/z.txt"))
                          (error (e)
                            ;; not even bothered with locks! We just need to
                            ;; cache one error
                            (push e errors))))))))
         (mapc #'bt:join-thread threads))
       (assert-that
        errors
        (has-length 0))))))

(test safe-ensure-directories-exist-when-actually-cant-write-dir
  (signals
      file-error
    (safe-ensure-directories-exist "/foo/car/bar.txt")))
