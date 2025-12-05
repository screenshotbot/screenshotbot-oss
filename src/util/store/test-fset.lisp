;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/store/test-fset
  (:use #:cl
        #:fiveam)
  (:import-from #:bknr.datastore
                #:decode
                #:encode))
(in-package :util/store/test-fset)


(util/fiveam:def-suite)

(def-fixture state ()
  (let ((map (fset:with
              (fset:with
               (fset:empty-map)
               "foo" 2)
              "bar" 3))
        (stream (flex:make-in-memory-output-stream)))
    (flet ((decode-back ()
             (let ((stream (flex:make-in-memory-input-stream
                            (flex:get-output-stream-sequence stream))))
               (decode stream))))
     (&body))))

(test preconditions
  (with-fixture state ()
    (encode map stream)
    (is (fset:equal?
         (decode-back)
         map))))

(test empty-map
  (with-fixture state ()
    (encode (fset:empty-map) stream)
    (is (fset:equal?
         (decode-back)
         (fset:empty-map)))))

(test large-map-decoding
  "I couldn't reliably get this test to fail when using TCO"
  (let ((large-map (loop with map = (fset:empty-map)
                        for i from 1 to 1000
                        do (setf map (fset:with map i (* i 2)))
                        finally (return map)))
        (stream (flex:make-in-memory-output-stream)))
    (encode large-map stream)
    (let ((stream (flex:make-in-memory-input-stream
                   (flex:get-output-stream-sequence stream))))
      (is (fset:equal?
           (decode stream)
           large-map)))))

