;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-random-port
  (:use #:cl
        #:fiveam)
  (:import-from #:util/random-port
                #:*ports*
                #:with-random-port))
(in-package :util/tests/test-random-port)

(util/fiveam:def-suite)

(test test-happy-path ()
  (let ((calledp nil))
    (with-random-port (port)
      (is (> port 10000))
      (setf calledp t))
    (is-true calledp)
    (is (eql 0 (hash-table-count *ports*)))))

(test test-we-dont-pick-something-weve-seen ()
  (dotimes (i 5)
   (let ((*ports* (make-hash-table)))

     (loop for i from 0 to 35000
           do (setf (gethash i *ports*) t))

     (let ((calledp nil))
       (with-random-port (port)
         (is (> port 35000))
         (setf calledp t))
       (is-true calledp)))))
