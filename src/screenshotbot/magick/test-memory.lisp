;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/magick/test-memory
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/magick/memory
                #:realloc
                #:free
                #:malloc)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/magick/test-memory)


(util/fiveam:def-suite)

(test allocate
  (let ((mem (malloc 20)))
    (unwind-protect
         (is (not (fli:null-pointer-p mem)))
      (free mem))))

(test realloc
  (let ((mem (malloc 20)))
    (let ((mem (realloc mem 40)))
      (unwind-protect
           (is (not (fli:null-pointer-p mem)))
        (free mem)))))
