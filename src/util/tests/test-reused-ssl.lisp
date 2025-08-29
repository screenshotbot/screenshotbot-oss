(defpackage :util/tests/test-reused-ssl
  (:use #:cl
        #:fiveam)
  (:import-from #:util/reused-ssl
                #:tracked-stream-key
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
                #:has-length)
  (:import-from #:util/request
                #:http-request))
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

(defvar *enable-network-tests-p* nil)
;; (setf *enable-network-tests-p* t)

(def-fixture network-state ()
  (when *enable-network-tests-p*
    (let ((engine (make-instance 'reused-ssl-engine)))
     (&body))))

(test making-requests-without-reuse-uss
  (with-fixture network-state ()
    (dotimes (i 10)
     (is (equal "leader"
                (http-request
                 "https://screenshotbot.io/raft-state"
                 :want-string t
                 :engine engine))))))
 
(test making-requests-with-reuse-ssl
  (with-fixture network-state ()
    (with-reused-ssl (engine)
      (dotimes (i 10)
       (is (equal "leader"
                  (http-request
                   "https://screenshotbot.io/raft-state"
                   :want-string t
                   :engine engine)))))))

(test tracked-stream-is-only-closed-once
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
              (close stream)
              (close stream))
            (is-true (find-connection reuse-context "example.com"))
            (is-false (find-connection reuse-context "example.com"))))))))

(test tracked-stream-key
  (is (equal "https://example.com"
             (tracked-stream-key
              (puri:uri "https://example.com/dfdf/sdfd")))))

(test want-string-test
  (with-fixture network-state ()
    (with-reused-ssl (engine)
      (multiple-value-bind (response status-code)
          (http-request
           "https://screenshotbot.io/raft-state"
           :want-string t
           :engine engine)
        (is (stringp response))
        (is (equal "leader" response))
        (is (= 200 status-code))))))


