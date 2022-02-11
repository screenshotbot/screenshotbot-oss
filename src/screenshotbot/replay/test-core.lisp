;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/test-core
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/replay/core
                #:read-srcset
                #:rewrite-css-urls)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/replay/test-core)


(util/fiveam:def-suite)


(test url-rewriting
  (let ((css "foo {
background: url(https://google.com)
}"))
    (is
     (equal
      "foo {
background: url(shttps://google.com?f=1)
}"
      (rewrite-css-urls css (lambda (url)
                              (format nil "s~a?f=1" url))))))
    (let ((css "foo {
background: url('https://google.com')
}"))
    (is
     (equal
      "foo {
background: url(shttps://google.com?f=1)
}"
      (rewrite-css-urls css (lambda (url)
                              (format nil "s~a?f=1" url)))))))

(test read-srcset
  (is (eql nil (read-srcset " ")))
  (is (equal `(("foo" . "20w"))
             (read-srcset "foo 20w")))
  (is (equal `(("foo" . "20w"))
             (read-srcset "  foo    20w   ")))
  (is (equal `(("foo" . "20w")
               ("bar" . "30w"))
             (read-srcset "  foo    20w  ,bar 30w ")))
  (is (equal `(("foo" . "20w")
               ("bar,0" . "30w"))
             (read-srcset "  foo    20w  ,bar,0 30w "))))
