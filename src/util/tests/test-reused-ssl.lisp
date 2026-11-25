(defpackage :util/tests/test-reused-ssl
  (:use #:cl
        #:fiveam)
  (:import-from #:util/reused-ssl
                #:tracked-stream-key
                #:trim-old-connections
                #:connection
                #:connection-stream
                #:connections
                #:find-connection
                #:*reuse-contexts*
                #:tracked-stream
                #:with-reused-ssl
                #:push-connection
                #:reuse-context
                #:reused-ssl-engine)
  (:import-from #:nursery
                #:with-nursery
                #:start-soon)
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

(defclass failing-stream (trivial-gray-streams:fundamental-input-stream)
  ()
  (:documentation "A stream that can't be closed, like a connection that the peer has
already dropped."))

(defmethod close ((self failing-stream) &key abort)
  (declare (ignore abort))
  (error "could not close the stream"))

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

(test the-reuse-context-is-shared-with-nursery-tasks
  (with-fixture state ()
    (with-reused-ssl (engine :reuse-context reuse-context)
      (let ((seen nil)
            (lock (bt:make-lock)))
        (with-nursery (nursery)
          (dotimes (i 3)
            (start-soon nursery
                        (lambda ()
                          (let ((context (reuse-context engine)))
                            (bt:with-lock-held (lock)
                              (push context seen)))))))
        (assert-that seen (has-length 3))
        (is (every (lambda (context) (eql context reuse-context)) seen))))))

(test a-connection-is-never-checked-out-twice
  (with-fixture state ()
    (with-reused-ssl (engine :reuse-context reuse-context)
      (let ((streams (loop repeat 4 collect (make-string-input-stream "")))
            (in-use (make-hash-table))
            (double-checkouts 0)
            (lock (bt:make-lock)))
        (dolist (stream streams)
          (push-connection reuse-context "example.com" stream))
        (with-nursery (nursery)
          (dotimes (i 8)
            (start-soon nursery
                        (lambda ()
                          (dotimes (j 2000)
                            (let ((connection (find-connection reuse-context "example.com")))
                              (when connection
                                (let ((stream (connection-stream connection)))
                                  (bt:with-lock-held (lock)
                                    (if (gethash stream in-use)
                                        (incf double-checkouts)
                                        (setf (gethash stream in-use) t)))
                                  ;; Give another thread a chance to grab it too
                                  (bt:thread-yield)
                                  (bt:with-lock-held (lock)
                                    (remhash stream in-use))
                                  (push-connection reuse-context "example.com" stream)))))))))
        (is (eql 0 double-checkouts))
        (is (null (set-exclusive-or
                   streams
                   (mapcar #'connection-stream
                           (gethash "example.com" (connections reuse-context))))))))))

(test closing-a-tracked-stream-after-the-context-is-gone
  (with-fixture state ()
    (let ((input (make-string-input-stream "foobar"))
          (stream nil))
      (with-reused-ssl (engine :reuse-context reuse-context)
        (setf stream (make-instance 'tracked-stream
                                    :reuse-context reuse-context
                                    :domain "example.com"
                                    :delegate input
                                    :reusable-stream input)))
      (is (open-stream-p input))
      (finishes (close stream))
      (is-false (open-stream-p input)))))

(test a-connection-that-fails-to-close-does-not-fail-the-trim
  (with-fixture state ()
    (with-reused-ssl (engine :reuse-context reuse-context)
      (let ((good (make-string-input-stream "")))
        ;; The failing stream is closed first
        (dolist (stream (list good (make-instance 'failing-stream)))
          (push
           (make-instance 'connection
                          :last-use-time (- (get-universal-time) 30)
                          :stream stream
                          :domain "example.com")
           (gethash "example.com" (connections reuse-context))))
        (finishes (find-connection reuse-context "example.com"))
        (is-false (open-stream-p good))))))

(test a-connection-that-fails-to-close-does-not-fail-the-cleanup
  (with-fixture state ()
    (let ((good (make-string-input-stream "")))
      (finishes
        (with-reused-ssl (engine :reuse-context reuse-context)
          (push-connection reuse-context "example.com" good)
          (push-connection reuse-context "example.com" (make-instance 'failing-stream))))
      (is-false (open-stream-p good)))))

(test a-late-connection-that-fails-to-close
  (with-fixture state ()
    (let ((context nil))
      (with-reused-ssl (engine :reuse-context reuse-context)
        (setf context reuse-context))
      (finishes
        (push-connection context "example.com" (make-instance 'failing-stream))))))

(test closing-a-tracked-stream-returns-the-reusable-stream
  "When the response is decoded (e.g. gzip), the delegate is a wrapper
around the connection, that's only good for this one response."
  (with-fixture state ()
    (with-reused-ssl (engine :reuse-context reuse-context)
      (let* ((underlying (make-string-input-stream "foobar"))
             (wrapper (make-concatenated-stream underlying))
             (stream (make-instance 'tracked-stream
                                    :reuse-context reuse-context
                                    :domain "example.com"
                                    :delegate wrapper
                                    :reusable-stream underlying)))
        (close stream)
        (is (eql underlying
                 (connection-stream
                  (find-connection reuse-context "example.com"))))))))
