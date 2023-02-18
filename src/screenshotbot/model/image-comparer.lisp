;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/image-comparer
  (:use #:cl)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/model/image
                #:base-image-comparer)
  (:export
   #:make-image-comparer))
(in-package :screenshotbot/model/image-comparer)

(defmethod make-image-comparer (run)
  (make-instance 'base-image-comparer))
