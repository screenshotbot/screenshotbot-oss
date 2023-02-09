;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-lparallel
  (:use #:cl
        #:fiveam)
  (:import-from #:util/lparallel
                #:while-promises
                #:*sleep-time*
                #:wait-for-many
                #:bind-future)
  (:import-from #:util/testing
                #:with-global-kernel)
  (:import-from #:lparallel.promise
                #:future)
  (:import-from #:lparallel.promise
                #:force))
(in-package :util/tests/test-lparallel)

(util/fiveam:def-suite)

(test simple-bind-fn
  (with-global-kernel ()
    (let ((f1 (future 2)))
      (is (equal 3 (force (bind-future f1 (lambda (x) (+ 1 x))))))
      (is (equal 3 (force (bind-future f1 (lambda (x) (+ 1 x)))))))))

(test wait-for-many
  (with-global-kernel ()
    (let ((f1 (future 2))
          (p1 (lparallel:promise)))
      (is (eql f1 (wait-for-many (list f1 p1))))
      (is (eql f1 (wait-for-many (list f1)))))))

(test while-promises-happy-path
  (let ((*sleep-time* 0.1))
   (with-global-kernel ()
     (let ((f1 (future 2))
           (p1 (lparallel:promise)))
       (let ((results))
         (future
           (while-promises (f1 p1)
             (push (list (lparallel:fulfilledp f1)
                         (lparallel:fulfilledp p1))
                   results)))
         (loop until results
               for i from 0 to 100
               do (sleep 0.1))
         (lparallel:fulfill p1 10)
         (loop until (eql 2 (length results))
               for i from 0 to 100
               do (sleep 0.1))
         (is (equal
              `((t t)
                (t nil))
              results)))))))
