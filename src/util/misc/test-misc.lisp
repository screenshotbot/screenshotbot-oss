;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/misc/tests/test-misc
  (:use #:cl
        #:fiveam)
  (:import-from #:util/misc
                #:ntrim-list
                #:random-selection
                #:safe-with-open-file
                #:relpath
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
(in-package :util/misc/tests/test-misc)


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

(test or-setf-thread-safety
  (loop for i from 0 to 100
        do
           (let ((ctr 0)
                 (val nil))
             (let ((threads (loop for i from 0 to 10
                                  collect (bt:make-thread
                                           (lambda ()
                                             (util:or-setf
                                              val
                                              (incf ctr)
                                              :thread-safe t))))))
               (mapc #'bt:join-thread threads)
               (is (eql 1 ctr))))))


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

#-screenshotbot-oss
(test safe-ensure-directories-exist-when-actually-cant-write-dir
  #-windows
  (unless (equal "root" (uiop:getenv "USER"))
    (signals
        file-error
      (safe-ensure-directories-exist "/foo/car/bar.txt"))))

(defun pathname-equal (a b)
  (string= (namestring a) (namestring b)))

(test relpath
  (let ((dir #P "/etc/bar/"))
    (let ((subdir (path:catdir dir "foo/")))
      (is (pathname-equal #P"foo/" (relpath subdir dir)))
      (is (pathname-equal #P "../foo/" (relpath subdir (path:catdir dir "bleh/"))))
      (is (pathname-equal #P "../../foo/" (relpath subdir (path:catdir dir "bleh/dfd/"))))
      (is (pathname-equal #P "../foo.txt" (relpath "/etc/bar/foo.txt" "/etc/bar/car/" ))))))

(test safe-with-open-file
  (tmpdir:with-tmpdir (dir)
    (safe-with-open-file (stream (path:catfile dir "foo.txt") :direction :output)
      (write-string "arnold" stream))
    (safe-with-open-file (stream (path:catfile dir "foo.txt") :direction :input)
      (is (equal "arnold" (read-line stream))))))

(test random-select
  (dotimes (i 100)
    (assert
     (random-selection (list 1 2 3 4 5 6 7 8 9 10) :weight (lambda (x) (* x x))))))

(test ntrim-list
  (let ((x (list 1 2 3 4 5)))
    (ntrim-list x 3)
    (is (equal (list 1 2 3) x))
    (finishes (ntrim-list nil 3))))
