;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-async
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/async
                #:magick-future
                #:sb/future)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-async)


(util/fiveam:def-suite)

(test sb/future-happy-path
  (let ((val (lparallel:force (sb/future () :done))))
    (is (eql :done val))))

(test magick-future-happy-path
  (is (eql 2
           (lparallel:force
            (magick-future () (+ 1 1))))))
