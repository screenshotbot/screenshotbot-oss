;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-threading
  (:use #:cl
        #:fiveam
        #:util/threading)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-threading)


(util/fiveam:def-suite)

(test simple-create-thread
  (let ((var nil))
    (let ((thread (util:make-thread
                   (lambda ()
                     (setf var t)))))
      (bt:join-thread thread)
      (is-true var))))


#+lispworks
(test safe-interrupt
  (let* ((ctr 0)
         (max-ctr 10)
         (callback-called-p nil)
         (thread (bt:make-thread
                  (lambda ()
                    (with-safe-interruptable (:on-quit (lambda ()
                                                         (setf callback-called-p t)))
                      (loop for i below max-ctr
                            do
                               (incf ctr)
                               (safe-interrupt-checkpoint)
                               (sleep 0.1)))))))
    (safe-interrupt thread)
    (bt:join-thread thread)
    (is-true callback-called-p)
    (is (< ctr (/ max-ctr 2)))
    (pass)))
