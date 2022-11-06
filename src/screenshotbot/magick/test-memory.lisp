;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/magick/test-memory
  (:use #:cl
        #:fiveam)
  (:local-nicknames (#:a #:alexandria)
                    #-lispworks
                    (#:fli #:util/fake-fli)))
(in-package :screenshotbot/magick/test-memory)

(util/fiveam:def-suite)


(fli:define-foreign-function (malloc "screenshotbot_malloc")
  ((size :size-t))
  :result-type (:pointer :void))

(fli:define-foreign-function (realloc "screenshotbot_realloc")
  ((ptr (:pointer :void))
   (size :size-t))
  :result-type (:pointer :void))

(fli:define-foreign-function (free "screenshotbot_free")
  ((ptr (:pointer :void)))
  :result-type :void)

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

(test big-realloc
  (let ((mem (malloc 20)))
    (let ((mem (realloc mem (* 4 1024 1024))))
      (unwind-protect
           (is (not (fli:null-pointer-p mem)))
        (free mem)))))

(test big-then-small-realloc
  (let ((mem (malloc 20)))
    (let ((mem (realloc mem (* 4 1024 1024))))
      (let ((mem (realloc mem 1024)))
       (unwind-protect
            (is (not (fli:null-pointer-p mem)))
         (free mem))))))

(test logging-allocation-happy-path
  (let ((mem (malloc (* 20 1024 1024))))
    (free mem)))
