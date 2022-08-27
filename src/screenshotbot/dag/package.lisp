;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :dag
  (:use #:cl
        #:alexandria)
  (:export #:dag
           #:add-commit
           #:commit
           #:merge-dag
           #:parents
           #:ancestorp
           #:get-commit
           #:write-to-stream
           #:read-from-stream
           #:ordered-commits
           #:sha
           #:author))
(in-package :dag)
