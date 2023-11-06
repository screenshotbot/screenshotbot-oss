;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-commit-graph-v2
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/commit-graph-v2
                #:commit-graph-subsystem
                #:*map*)
  (:import-from #:util/store/store
                #:with-test-store))
(in-package :screenshotbot/model/test-commit-graph-v2)


(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*map* (fset:empty-map)))
    (&body)))

(def-fixture simple-cleanup ()
  (unwind-protect
       (&body)
    (setf *map* (fset:empty-map))))

(test saving-and-restoring-subsystem
  (with-fixture state ()
    (tmpdir:with-tmpdir (dir)
      (with-test-store (:dir dir
                        :globally t)
        (setf *map*
              (fset:with
               *map*
               "foo"
               "bar"))
        (util:safe-snapshot))
      (is (fset:equal? *map* (fset:empty-map)))
      (with-test-store (:dir dir
                        :globally t)
        (is (fset:equal? (fset:with
                          (fset:empty-map)
                          "foo"
                          "bar")
                         *map*))))))
