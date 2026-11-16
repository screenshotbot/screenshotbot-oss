;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-memory
  (:use #:cl
        #:fiveam)
  (:import-from #:util/memory
                #:arena-size
                #:malloc-info
                #:format-mb
                #:parse-room-output
                #:room-table
                #:process-mem-usage)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-memory)


(util/fiveam:def-suite)

(test process-mem-usage
  (is (> (process-mem-usage) 0)))

(test malloc-info
  (finishes
    (malloc-info)))

(test arena-size
  (multiple-value-bind (arena hblkhd)
      (arena-size)
    (is (>= arena 0))
    (is (>= hblkhd 0))))


(defparameter *room-output*
  " > Generation 1: 31350784 (0x1DE6000)
       Cons               8968544 (0x88D960)
       Weak               96 (0x60)
 -- Segments:
       Cons               4090008800 - 4090B59000
================================
 > Generation 0: 9618912 (0x92C5E0)
       Cons               1709824 (0x1A1700)

Total allocated 103609896 (0x62CF628), total size 180477952 (0xAC1E000)
")

(test parse-room-output
  (multiple-value-bind (gens allocated size)
      (parse-room-output *room-output*)
    (is (equal '((1 31350784 (("Cons" . 8968544) ("Weak" . 96)))
                 (0 9618912 (("Cons" . 1709824))))
               gens))
    (is (eql 103609896 allocated))
    (is (eql 180477952 size))))

(test format-mb
  (is (equal "0.0" (format-mb 0)))
  (is (equal "1.5" (format-mb (* 1536 1024))))
  (is (equal "2048.0" (format-mb (* 2 1024 1024 1024)))))

(test room-table
  (let ((output (with-output-to-string (s)
                  (room-table :stream s))))
    (is (search "Gen" output))
    (is (search "Total allocated" output))))
