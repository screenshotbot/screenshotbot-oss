;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :remark/test-search
  (:use #:cl
        #:fiveam)
  (:import-from #:remark/nodes
                #:find-node
                #:defpage
                #:defsection
                #:deftoplevel)
  (:import-from #:remark/search
                #:search-remarks)
  (:import-from #:fiveam-matchers/lists
                #:has-item)
  (:import-from #:fiveam-matchers/core
                #:assert-that))
(in-package :remark/test-search)

(util/fiveam:def-suite)

(deftoplevel *toplevel* ()
  one)

(defsection one ()
  my-page)

(defpage my-page ()
  "foo bar")

(test test-search-has-path ()
  (assert-that (search-remarks *toplevel* "foo")
               (has-item (find-node 'my-page))))
