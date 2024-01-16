;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/test-config
  (:use #:cl
        #:fiveam)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:server/config
                #:load-config
                #:*config-file*)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that))
(in-package :server/test-config)

(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*installation* :none)
        (*config-file* nil))
    (&body)))

(defclass fake-installation ()
  ())

(test simple-load-config ()
  (with-fixture state ()
   (uiop:with-temporary-file (:pathname p :stream s)
     (let ((*package* (find-package :CL)))
       (write `(make-instance 'fake-installation) :stream s))
     (finish-output s)
     (let ((*config-file* p))
       (load-config))
     (assert-that *installation*
                  (has-typep 'fake-installation)))))
