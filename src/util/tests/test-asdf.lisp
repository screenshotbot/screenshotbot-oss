;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-asdf
  (:use #:cl
        #:fiveam)
  (:import-from #:util
                #:%asdf-relpath))
(in-package :util/tests/test-asdf)

(util/fiveam:def-suite)

(test asdf-relpath-for-file-that-exists
  (let ((res (%asdf-relpath
               (asdf:system-relative-pathname
                :util
                "fixture/file.txt"))))
    (is
     (equalp
      (make-pathname
       :host nil
       :device nil
       :version nil
       :defaults #P"src/util/fixture/file.txt")
      res))))

(test asdf-relpath-for-file-that-does-not-exists
  (let ((res (%asdf-relpath
               (asdf:system-relative-pathname
                :util
                "fixture/file-does-not-exist.txt"))))
    (is
     (equalp
      (make-pathname
       :host nil
       :device nil
       :version nil
       :defaults #P"src/util/fixture/file-does-not-exist.txt")
      res))))


