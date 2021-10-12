;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :util/tests/test-store
    (:use #:cl
          #:fiveam
          #:alexandria)
  (:import-from #:util
                #:parse-timetag
                #:*object-store*
                #:object-store)
  (:import-from #:local-time
                #:timestamp=))


(util/fiveam:def-suite)

(test object-store
  (tmpdir:with-tmpdir (tmp)
    (let ((*object-store* (namestring (path:catfile tmp "dummy/"))))
      (is (pathnamep (object-store)))
      (is (path:-d (object-store)))))
  (tmpdir:with-tmpdir (tmp)
    (let ((*object-store* (namestring (path:catfile tmp "dummy"))))
      (is (pathnamep (object-store)))
      (is (path:-d (object-store)))))

  #+nil ;; bad test that modifies global state
  (let ((*object-store* "~/zoidberg/bar/"))
    (is (equal #P "~/zoidberg/bar/" (object-store)))
    (is (path:-d (object-store)))))


(test parse-timetag
  (is
   (timestamp=
    (local-time:parse-timestring "2021-10-09T06:00:00")
    (parse-timetag "20211009T060000"))))
