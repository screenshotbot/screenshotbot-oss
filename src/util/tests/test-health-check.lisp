;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-health-check
  (:use #:cl
        #:fiveam
        #:util/health-check)
  (:import-from #:util/health-check
                #:*checks*)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-health-check)


(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*checks* nil))
    (uiop:with-temporary-file (:pathname tmp-output
                               :stream tmp)
      (&body))))

(test simple-definition
  (with-fixture state ()
    (def-health-check foo-bar ()
      (values))
    (is (eql 1 (length *checks*)))
    (def-health-check car ()
      (values))
    (is (eql 2 (length *checks*)))
    (def-health-check foo-bar ()
      (values))
    (is (eql 2 (length *checks*)))))

(test call-health-checks
  (with-fixture state ()
    (def-health-check foo-bar ()
      (values))
    (is-true (run-health-checks :out tmp))
    (def-health-check bad ()
      (error "what"))
    (multiple-value-bind (res failures) (run-health-checks :out tmp)
      (is-false res)
      (is (equal '(bad) failures)))))
