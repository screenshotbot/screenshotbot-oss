;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-throttler
  (:use #:cl
        #:fiveam)
  (:import-from #:util/throttler
                #:keyed-throttler
                #:%tokens
                #:throttled-error
                #:throttled-funcall
                #:throttler
                #:with-throttling)
  (:import-from #:local-time
                #:universal-to-timestamp))
(in-package :util/tests/test-throttler)


(util/fiveam:def-suite)

(defclass my-throttler (throttler)
  ())

(def-fixture state ()
  (let ((throttler (make-instance 'my-throttler :tokens 20)))
    (&body)))

(test simple-call
  (with-fixture state ()
    (is (eql :done
             (with-throttling (throttler)
               :done)))))

(test simple-call-with-key
  (with-fixture state ()
    (let ((throttler (make-instance 'keyed-throttler
                                    :tokens 100)))
      (is (eql :done
               (with-throttling (throttler :key 20)
                 :done))))))

(test throttles
  (with-fixture state ()
    (let ((throttler (make-instance 'my-throttler :tokens 10
                                                  :period 1000
                                                  :now 1000))
          (ctr 0))
      (dotimes (i 10)
        (throttled-funcall throttler (lambda ()
                                       (incf ctr))
                           :now (+ 1000 i)))
      (is (eql 10 ctr))
      (is (< (%tokens throttler) 1))
      (signals throttled-error
       (throttled-funcall throttler (lambda ()
                                      (incf ctr))
                          :now 1013))
      (throttled-funcall throttler (lambda ()
                                     (incf ctr))
                         :now 1102))))

(test we-gracefully-add-more-tokens
  "This is different from the strict N request per T time."
  (with-fixture state ()
    (let ((throttler (make-instance 'my-throttler :tokens 10
                                                  :period 100
                                                  :now 1000))
          (ctr 0))
      (dotimes (i 10)
        (throttled-funcall throttler (lambda ()
                                       (incf ctr))
                           :now (+ 1000 i)))
      (is (eql 10 ctr))
      (is (< (%tokens throttler) 1))

      (throttled-funcall throttler (lambda ()
                                     (incf ctr))
                         :now 1030))))


(test we-dont-exceed-token-limit
  (with-fixture state ()
    (let ((throttler (make-instance 'my-throttler :tokens 10
                                                  :period 1000
                                                  :now 0))
          (ctr 0))
      (dotimes (i 10)
        (throttled-funcall throttler (lambda ()
                                       (incf ctr))
                           :now (+ 1000 i)))
      (is (eql 10 ctr))
      (is (< (%tokens throttler) 1))
      (signals throttled-error
       (throttled-funcall throttler (lambda ()
                                      (incf ctr))
                          :now 1013))
      (throttled-funcall throttler (lambda ()
                                     (incf ctr))
                         :now 1102))))


(test throttles-with-keyed-throttler
  (with-fixture state ()
    (let ((throttler (make-instance 'keyed-throttler :tokens 10
                                                     :period 1000))
          (ctr 0))
      (dotimes (i 10)
        (throttled-funcall throttler (lambda ()
                                       (incf ctr))
                           :now (+ 1000 i)
                           :key "foo"))
      (is (eql 10 ctr))
      (signals throttled-error
       (throttled-funcall throttler (lambda ()
                                      (incf ctr))
                          :now 1013
                          :key "foo"))
      (throttled-funcall throttler (lambda ()
                                     (incf ctr))
                         :now 1013
                         :key "bar")
      (throttled-funcall throttler (lambda ()
                                     (incf ctr))
                         :now 1102
                         :key "foo"))))
