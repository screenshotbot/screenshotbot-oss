(defpackage :util/tests/test-reused-ssl
  (:use #:cl
        #:fiveam)
  (:import-from #:util/reused-ssl
                #:trim-old-connections
                #:connection
                #:connections
                #:find-connection
                #:*reuse-contexts*
                #:tracked-stream
                #:with-reused-ssl
                #:reused-ssl-engine)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length))
(in-package :util/tests/test-reused-ssl)

(util/fiveam:def-suite)

(def-fixture state ()
  (let ((engine (make-instance 'reused-ssl-engine)))
    (&body)))

(test simple-invocation
  (with-fixture state ()
    (finishes
      (with-reused-ssl (engine)))))

(test ensure-the-stream-gets-reused
  (with-fixture state ()
   (with-reused-ssl (engine)
     (uiop:with-temporary-file (:pathname p :stream s :direction :output)
       (write-string "foobar
carbar" s)
       (close s)
       (let ((reuse-context (assoc-value *reuse-contexts* engine)))
        (with-open-file (input p)
          (let ((stream (make-instance 'tracked-stream
                                       :reuse-context reuse-context
                                       :domain "example.com"
                                       :delegate input
                                       :reusable-stream input)))
            (is (equal "foobar" (read-line input)))
            (close stream))
          (is-false (find-connection reuse-context "foobar.com"))
          (is-true (find-connection reuse-context "example.com"))))))))

(test cleanup-old-connections
  (with-fixture state ()
    (with-reused-ssl (engine :reuse-context reuse-context)
      (uiop:with-temporary-file (:stream s)
        (push
         (make-instance 'connection
                        :last-use-time (- (get-universal-time) 30)
                        :stream s
                        :domain "example.com")
         (gethash "example.com" (connections reuse-context)))
        (trim-old-connections reuse-context)
        (assert-that
         (gethash "example.com" (connections reuse-context))
         (has-length 0))))))
