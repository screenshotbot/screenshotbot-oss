;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-misc
  (:use #:cl
        #:fiveam)
  (:import-from #:util
                #:or-setf)
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
