;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :dag
  (:use #:cl
        #:alexandria)
  (:import-from #:bknr.datastore
                #:encode-integer)
  (:import-from #:bknr.datastore
                #:%decode-integer)
  (:import-from #:bknr.datastore
                #:encode-object)
  (:import-from #:bknr.datastore
                #:decode)
  (:import-from #:bknr.datastore
                #:encode-string)
  (:import-from #:bknr.datastore
                #:encode)
  (:import-from #:alexandria
                #:curry)
  (:import-from #:util/simple-queue
                #:dequeue
                #:queue-emptyp
                #:enqueue
                #:make-queue)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:util/store/encodable
                #:encodable)
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
           #:author
           #:merge-base
           #:best-path
           #:dag-difference
           #:clone-dag
           #:all-commits))
(in-package :dag)
