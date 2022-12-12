;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/test-server
  (:use #:cl
        #:fiveam)
  (:import-from #:server
                #:with-lparallel-kernel)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:local-nicknames (#:a #:alexandria)))
(in-package :server/test-server)

(util/fiveam:def-suite)

(test with-lparallel-kernel
  (let (val warning)
    (with-lparallel-kernel ()
      (setf val (lparallel:force
                 (lparallel:future :done))))
    (is (eql :done val))))

(test with-lparallel-kernel-when-no-jobs-were-created
  (let (val warning)
    (with-lparallel-kernel ()
      nil)
    (pass)))
