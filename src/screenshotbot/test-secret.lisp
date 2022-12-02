;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-secret
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/secret
                #:read-secrets
                #:*secret-file*)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-secret)


(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*secret-file* (asdf:system-relative-pathname :screenshotbot "fixture/secrets.xml")))
    (&body)))

(test simple-reading
  (with-fixture state ()
    (assert-that (read-secrets)
                 (has-length 2))))
