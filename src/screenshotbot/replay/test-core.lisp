;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/test-core
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/replay/core
                #:*fetch-context*
                #:url
                #:assets
                #:snapshot
                #:should-rewrite-url-p
                #:read-srcset
                #:push-asset
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

(test should-rewrite-url-p
  (is-true (should-rewrite-url-p "https://foobar.com/foo"))
  (is-false (should-rewrite-url-p "moz-extension://foobar.com/foo")))


(test push-asset-is-correctly-cached
  (let ((*fetch-context* nil))
   (tmpdir:with-tmpdir (tmpdir)
     (cl-mock:with-mocks ()

       (cl-mock:if-called 'dex:get
                          (lambda (url &rest args)
                            (values
                             (flexi-streams:make-in-memory-input-stream
                              #())
                             200
                             (make-hash-table :test #'equal))))

       (let* ((snapshot (make-instance 'snapshot :tmpdir tmpdir))
              (rand (random 10000000000))
              (font (format nil "https://screenshotbot.io/assets/fonts/metropolis/Metropolis-Bold-arnold.otf?id=~a" rand))
              (html (format nil "https://screenshotbot.io/?id=~a" rand)))

         (push-asset snapshot (quri:uri html) nil)
         (is (equal 1 (length (assets snapshot))))
         (push-asset snapshot (quri:uri font)  t)
         (is (equal 2 (length (assets snapshot))))
         (is (equal font
                    (url (car (Assets snapshot)))))
         (push-asset snapshot (quri:uri font)  t)

         (is (equal 2 (length (assets snapshot))))
         (push-asset snapshot (quri:uri html) nil)
         (is (equal 2 (length (assets snapshot))))
         (push-asset snapshot (quri:uri font)  t)
         (is (equal 2 (length (assets snapshot)))))))))
