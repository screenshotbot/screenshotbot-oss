;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(defpackage :core/config/test-model
  (:use #:cl
        #:fiveam)
  (:import-from #:core/config/model
                #:on-config-changed
                #:value-must-be-string
                #:config)
  (:import-from #:util/store/store
                #:with-test-store))
(in-package :core/config/test-model)

(util/fiveam:def-suite)

(defvar *changed*)

(def-fixture state ()
  (setf *changed* nil)
  (with-test-store ()
    (&body)))

(test simple-get-set-etc
  (with-fixture state ()
    (is (equal nil (config "foo.bar")))
    (setf (config "foo.bar") "car")
    (is (equal "car" (config "foo.bar")))
    (setf (config "foo.bar") "zoidberg")
    (is (equal "zoidberg" (config "foo.bar")))))

(test cant-set-integer
  (with-fixture state ()
    (signals value-must-be-string
      (setf (config "foo.bar") 15))
    (is (equal nil (config "foo.bar")))))

(defmethod on-config-changed ((key (eql :foo.bar.car.bar))
                              value)
  (setf *changed* value))

(test callback-is-called
  (with-fixture state ()
    (is (eql nil *changed*))
    (setf (config "foo.bar.car.bar")  "zoidberg")
    (is (equal "zoidberg" *changed*))))
