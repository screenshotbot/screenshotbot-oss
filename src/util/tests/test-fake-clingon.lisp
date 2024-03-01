;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-fake-clingon
  (:use #:cl
        #:fiveam)
  (:import-from #:clingon.options
                #:make-option)
  (:import-from #:util/fake-clingon
                #:make-fake-clingon))
(in-package :util/tests/test-fake-clingon)

(util/fiveam:def-suite)

(defun batch-args ()
  (list
   (make-option
    :string
    :long-name "batch"
    :description "sdfsdfds"
    :key :batch)
   (make-option
    :string
    :long-name "repo-url"
    :description "dfdfd"
    :key :repo-url)
   (make-option
    :string
    :long-name "pull-request"
    :description "xdfdfd"
    :key :pull-request)))

(test simple-invocation
  (let ((cmd (make-fake-clingon (batch-args)
                                :repo-url "foo")))
    (is (equal "foo" (clingon:getopt cmd :repo-url)))
    (signals simple-error
      (clingon:getopt cmd :foo))))
