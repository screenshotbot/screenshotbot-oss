;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/scim/test-dto
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/api/model
                #:decode-json)
  (:import-from #:screenshotbot/scim/dto
                #:external-user-user-name
                #:external-user))
(in-package :screenshotbot/scim/test-dto)


(util/fiveam:def-suite)

(def-fixture state ()
  (&body))

(test simple-parsing
  (with-fixture state ()
    (let ((obj (decode-json
                (uiop:read-file-string (asdf:system-relative-pathname :screenshotbot "scim/sample-schema.json"))
                'external-user)))
      (is (equal "bjensen" (external-user-user-name obj))))))

