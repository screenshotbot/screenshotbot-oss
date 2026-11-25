;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/curl/test-curl-request
  (:use #:cl
        #:fiveam)
  (:import-from #:util/request
                #:http-request
                #:connection-error
                #:timeout-error)
  (:import-from #:util/curl/curl-request
                #:libcurl-available-p
                #:*curl-engine*
                #:*libcurl-enabled-p*)
  (:import-from #:hunchentoot
                #:easy-acceptor
                #:define-easy-handler)
  (:import-from #:util/testing
                #:with-local-acceptor)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/curl/test-curl-request)

(util/fiveam:def-suite)

(defclass test-acceptor (easy-acceptor)
  ()
  (:default-initargs :name 'curl-test))

(defvar *body* nil)
(defvar *gzip* nil)
(defvar *persistent-connections-p* nil)

(define-easy-handler (text-handler :uri "/text"
                                   :acceptor-names '(curl-test)) ()
  (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
  "hello world ☃")

(define-easy-handler (dummy-content :uri "/dummy-content"
                                    :acceptor-names '(curl-test)) ()
  (when *gzip*
    (setf (hunchentoot:header-out :content-encoding) "gzip"))
  (setf (hunchentoot:content-type*) "application/octet-stream")
  *body*)

(define-easy-handler (echo-handler :uri "/echo"
                                   :acceptor-names '(curl-test)) ()
  (setf (hunchentoot:content-type*) "application/json")
  (json:encode-json-to-string
   `((:method . ,(string (hunchentoot:request-method*)))
     (:query . ,(hunchentoot:query-string*))
     (:foo . ,(hunchentoot:parameter "foo"))
     (:content-type . ,(hunchentoot:header-in* :content-type))
     (:authorization . ,(hunchentoot:header-in* :authorization))
     (:x-custom . ,(hunchentoot:header-in* :x-custom))
     (:accept-encoding . ,(hunchentoot:header-in* :accept-encoding))
     (:body . ,(let ((data (hunchentoot:raw-post-data :force-binary t)))
                 (when data
                   (flex:octets-to-string data :external-format :utf-8)))))))

(define-easy-handler (port-handler :uri "/port"
                                   :acceptor-names '(curl-test)) ()
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~a" (hunchentoot:remote-port*)))

(define-easy-handler (redirect-handler :uri "/redirect"
                                       :acceptor-names '(curl-test)) ()
  (hunchentoot:redirect "/text"))

(define-easy-handler (multiple-choices-handler :uri "/multiple-choices"
                                               :acceptor-names '(curl-test)) ()
  ;; libcurl follows any 3xx with a Location, not just 301/302/...
  (setf (hunchentoot:return-code*) 300)
  (setf (hunchentoot:header-out :location) "/text")
  (setf (hunchentoot:content-type*) "text/plain")
  "pick one")

(define-easy-handler (not-found-handler :uri "/not-found"
                                        :acceptor-names '(curl-test)) ()
  (setf (hunchentoot:return-code*) 404)
  (setf (hunchentoot:content-type*) "text/plain")
  "nope")

(define-easy-handler (slow-handler :uri "/slow"
                                   :acceptor-names '(curl-test)) ()
  (sleep 3)
  "done")

(define-easy-handler (large-handler :uri "/large"
                                    :acceptor-names '(curl-test)) ()
  (setf (hunchentoot:content-type*) "text/plain")
  (with-output-to-string (out)
    (dotimes (i 50000)
      (format out "line ~a~%" i))))

(def-fixture state ()
  (cond
    ((not (libcurl-available-p))
     (skip "libcurl is not available on this platform"))
    (t
     (unwind-protect
          (with-local-acceptor (host :prepare-acceptor-callback
                                     (lambda (acceptor)
                                       ;; Hunchentoot turns these off for the single threaded
                                       ;; taskmaster.
                                       (setf (hunchentoot:acceptor-persistent-connections-p acceptor)
                                             *persistent-connections-p*)))
              ('test-acceptor)
            (flet ((url (path)
                     (format nil "~a~a" host path))
                   (req (url &rest args)
                     (apply #'http-request url :engine *curl-engine* args)))
              (declare (ignorable #'url #'req))
              (unwind-protect
                   (&body)
                ;; The test acceptor is single threaded, so it can't
                ;; serve anybody else while we keep a connection to it
                ;; open.
                (util/curl/curl-request::clear-pool))))
       (setf *body* nil)
       (setf *gzip* nil)))))

(defun read-file-seq (file)
  (with-open-file (stream
                   (path:catfile
                    #.(asdf:system-source-directory :util)
                    file)
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((output (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (read-sequence output stream)
      output)))

(defun echo (response)
  (json:decode-json-from-string response))

(test simple-text-request
  (with-fixture state ()
    (multiple-value-bind (body code headers)
        (req (url "/text"))
      (is (equal "hello world ☃" body))
      (is (eql 200 code))
      (is (equal "text/plain; charset=utf-8"
                 (a:assoc-value headers :content-type))))))

(test returns-the-same-values-as-drakma
  (with-fixture state ()
    (multiple-value-bind (body code headers uri stream must-close reason)
        (util/curl/curl-request::curl-request (url "/redirect"))
      (is (equal "hello world ☃" body))
      (is (eql 200 code))
      (is (equal "text/plain; charset=utf-8"
                 (a:assoc-value headers :content-type)))
      (is (equal (url "/text") (puri:render-uri uri nil)))
      (is (null stream))
      (is (null must-close))
      (is (equal "OK" reason)))))

(test binary-request
  (with-fixture state ()
    (setf *body* (read-file-seq "test-file.txt"))
    (multiple-value-bind (content code headers)
        (req (url "/dummy-content")
             :force-binary t)
      (is (eql 200 code))
      (is (equalp (read-file-seq "test-file.txt") content))
      (is (typep content '(vector (unsigned-byte 8))))
      (is (equal "12" (a:assoc-value headers :content-length))))))

(test decoding-gzip
  (with-fixture state ()
    (setf *gzip* t)
    (setf *body* (read-file-seq "test-file-compressed.txt.gz"))
    (multiple-value-bind (content code headers)
        (req (url "/dummy-content")
             :accept-gzip t
             :decode-content t
             :force-binary t)
      (is (eql 200 code))
      (is (equalp (read-file-seq "test-file.txt") content))
      (is (equal "gzip" (a:assoc-value headers :content-encoding)))
      (is (equal "32" (a:assoc-value headers :content-length))))))

(test if-content-encoding-is-not-set-we-still-decode
  (with-fixture state ()
    (setf *gzip* t)
    (setf *body* (read-file-seq "test-file-compressed.txt.gz"))
    (is (equalp (read-file-seq "test-file.txt")
                (req (url "/dummy-content")
                     :decode-content t
                     :force-binary t)))))

(test if-decode-content-is-nil-we-dont-decode
  (with-fixture state ()
    (setf *gzip* t)
    (setf *body* (read-file-seq "test-file-compressed.txt.gz"))
    (is (equalp (read-file-seq "test-file-compressed.txt.gz")
                (req (url "/dummy-content")
                     :decode-content nil
                     :force-binary t)))))

(test we-dont-send-accept-encoding-unless-asked
  (with-fixture state ()
    (is (equal nil
               (a:assoc-value (echo (req (url "/echo") :decode-content t :want-string t))
                              :accept-encoding)))
    (is (equal "gzip"
               (a:assoc-value (echo (req (url "/echo") :accept-gzip t :decode-content t
                                                       :want-string t))
                              :accept-encoding)))))

(test get-parameters
  (with-fixture state ()
    (let ((res (echo (req (url "/echo?a=b")
                          :parameters '(("foo" . "bar baz"))
                          :want-string t))))
      (is (equal "GET" (a:assoc-value res :method)))
      (is (equal "a=b&foo=bar+baz" (a:assoc-value res :query)))
      (is (equal "bar baz" (a:assoc-value res :foo))))))

(test post-parameters
  (with-fixture state ()
    (let ((res (echo (req (url "/echo")
                          :method :post
                          :parameters '(("foo" . "bar&car"))
                          :want-string t))))
      (is (equal "POST" (a:assoc-value res :method)))
      (is (equal "bar&car" (a:assoc-value res :foo)))
      (is (equal "application/x-www-form-urlencoded"
                 (a:assoc-value res :content-type))))))

(test post-form-data
  (with-fixture state ()
    (let ((res (echo (req (url "/echo")
                          :method :post
                          :form-data t
                          :parameters '(("foo" . "bar"))
                          :want-string t))))
      (is (equal "bar" (a:assoc-value res :foo)))
      (is (str:starts-with-p "multipart/form-data; boundary="
                             (a:assoc-value res :content-type))))))

(test post-string-content
  (with-fixture state ()
    (let ((res (echo (req (url "/echo")
                          :method :post
                          :content "{\"a\": \"☃\"}"
                          :content-type "application/json"
                          :external-format-out :utf-8
                          :want-string t))))
      (is (equal "{\"a\": \"☃\"}" (a:assoc-value res :body)))
      (is (equal "application/json" (a:assoc-value res :content-type))))))

(test put-stream-with-content-length
  (with-fixture state ()
    (let* ((octets (flex:string-to-octets "hello from a stream"))
           (res (echo (req (url "/echo")
                           :method :put
                           :content (flex:make-in-memory-input-stream octets)
                           :content-length (length octets)
                           :content-type "application/octet-stream"
                           :want-string t))))
      (is (equal "PUT" (a:assoc-value res :method)))
      (is (equal "hello from a stream" (a:assoc-value res :body))))))

(test put-pathname
  (with-fixture state ()
    (let ((res (echo (req (url "/echo")
                          :method :put
                          :content (path:catfile #.(asdf:system-source-directory :util)
                                                 "test-file.txt")
                          :content-type "text/plain"
                          :want-string t))))
      (is (equal (flex:octets-to-string (read-file-seq "test-file.txt"))
                 (a:assoc-value res :body))))))

(test delete-with-no-content
  (with-fixture state ()
    (let ((res (echo (req (url "/echo")
                          :method :delete
                          :want-string t))))
      (is (equal "DELETE" (a:assoc-value res :method))))))

(test head-request
  (with-fixture state ()
    (multiple-value-bind (body code)
        (req (url "/text") :method :head)
      (is (eql 200 code))
      (is (null body)))))

(test headers-and-basic-auth
  (with-fixture state ()
    (let ((res (echo (req (url "/echo")
                          :basic-authorization '("user" "pass")
                          :additional-headers '((:x-custom . "custom-value"))
                          :want-string t))))
      (is (equal (format nil "Basic ~a" (cl-base64:string-to-base64-string "user:pass"))
                 (a:assoc-value res :authorization)))
      (is (equal "custom-value" (a:assoc-value res :x-custom))))))

(test headers-as-hash-table
  (with-fixture state ()
    (multiple-value-bind (body code headers)
        (req (url "/text") :headers-as-hash-table t)
      (declare (ignore body code))
      (is (equal "text/plain; charset=utf-8" (gethash "content-type" headers))))))

(test follows-redirects
  (with-fixture state ()
    (multiple-value-bind (body code)
        (req (url "/redirect"))
      (is (eql 200 code))
      (is (equal "hello world ☃" body)))))

(test redirect-nil-returns-the-redirect
  (with-fixture state ()
    (multiple-value-bind (body code headers)
        (req (url "/redirect") :redirect nil)
      (declare (ignore body))
      (is (eql 302 code))
      (is (str:ends-with-p "/text" (a:assoc-value headers :location))))))

(test non-success-response
  (with-fixture state ()
    (multiple-value-bind (body code)
        (req (url "/not-found") :want-string t)
      (is (eql 404 code))
      (is (equal "nope" body)))
    (signals util/request::bad-response-code
      (req (url "/not-found") :ensure-success t))))

(test want-stream
  (with-fixture state ()
    (multiple-value-bind (stream code headers)
        (req (url "/large") :want-stream t)
      (is (eql 200 code))
      (is (equal "text/plain; charset=utf-8" (a:assoc-value headers :content-type)))
      (with-open-stream (stream stream)
        (is (equal "line 0" (read-line stream)))
        (is (equal "line 1" (read-line stream)))
        (let ((rest (uiop:slurp-input-stream 'string stream)))
          (is (str:ends-with-p (format nil "line 49999~%") rest)))))))

(test want-stream-binary
  (with-fixture state ()
    (setf *body* (read-file-seq "test-file.txt"))
    (let ((stream (req (url "/dummy-content") :want-stream t :force-binary t)))
      (with-open-stream (stream stream)
        (let ((buf (make-array 100 :element-type '(unsigned-byte 8))))
          (let ((n (read-sequence buf stream)))
            (is (eql 12 n))
            (is (equalp (read-file-seq "test-file.txt")
                        (subseq buf 0 n)))
            (is (eql :eof (read-byte stream nil :eof)))))))))

(test closing-a-stream-early-doesnt-break-the-next-request
  (with-fixture state ()
    (let ((stream (req (url "/large") :want-stream t)))
      (is (equal "line 0" (read-line stream)))
      (close stream))
    (is (equal "hello world ☃" (req (url "/text"))))))

(test connections-are-reused
  (let ((*persistent-connections-p* t))
   (with-fixture state ()
    (let ((port-1 (req (url "/port")))
          (port-2 (req (url "/port"))))
      (is (equal port-1 port-2)))
    (let ((port-1 (with-open-stream (s (req (url "/port") :want-stream t))
                    (read-line s)))
          (port-2 (req (url "/port"))))
      (is (equal port-1 port-2))))))

(test read-timeout
  (with-fixture state ()
    (signals timeout-error
      (req (url "/slow") :read-timeout 1))))

(test connection-refused
  (with-fixture state ()
    (let ((url (format nil "http://127.0.0.1:~a/" (util/random-port:random-port))))
      (multiple-value-bind (body code)
          (req url :want-string t)
        (is (equal "" body))
        (is (eql 502 code)))
      (signals connection-error
        (req url :ensure-success t)))))

(test falls-back-to-drakma-for-unsupported-args
  (with-fixture state ()
    (let ((jar (make-instance 'drakma:cookie-jar)))
      (is (equal "hello world ☃" (req (url "/text") :cookie-jar jar))))))

(test can-be-disabled
  (with-fixture state ()
    (let ((*libcurl-enabled-p* nil))
      (is (equal "hello world ☃" (req (url "/text") :want-string t))))))

(test option-constants-match-libcurl
  (with-fixture state ()
    (do-symbols (sym :util/curl/curl-request)
      (let ((name (symbol-name sym)))
        (when (and (str:starts-with-p "+CURLOPT-" name)
                   (eql (symbol-package sym) (find-package :util/curl/curl-request)))
          (let ((curl-name (str:replace-all "-" "_" (subseq name 9 (1- (length name))))))
            (is (eql (symbol-value sym)
                     (util/curl/curl-request::option-id-by-name curl-name))
                "~a should be ~a"
                sym (util/curl/curl-request::option-id-by-name curl-name))))))))

(test read-sequence-respects-end
  (with-fixture state ()
    (setf *body* (read-file-seq "test-file.txt"))
    (let ((stream (req (url "/dummy-content") :want-stream t :force-binary t)))
      (with-open-stream (stream stream)
        (let ((buf (make-array 100 :element-type '(unsigned-byte 8)
                                   :initial-element 7)))
          (is (eql 5 (read-sequence buf stream :start 2 :end 5)))
          (is (equalp (subseq (read-file-seq "test-file.txt") 0 3)
                      (subseq buf 2 5)))
          (is (every (lambda (x) (eql x 7)) (subseq buf 5)))
          (is (every (lambda (x) (eql x 7)) (subseq buf 0 2))))))))

(test signals-http-redirecting
  (with-fixture state ()
    (let ((locations nil))
      (handler-bind ((drakma::http-redirecting
                       (lambda (c)
                         (push (drakma::http-redirect-location c) locations))))
        (is (equal "hello world ☃" (req (url "/redirect")))))
      (is (eql 1 (length locations)))
      (is (equal (url "/text")
                 (puri:render-uri (first locations) nil))))))

(test http-redirecting-handler-can-stop-the-redirect
  (with-fixture state ()
    (signals simple-error
      (handler-bind ((drakma::http-redirecting
                       (lambda (c)
                         (declare (ignore c))
                         (error "Not allowed to redirect there"))))
        (req (url "/redirect"))))
    ;; The handle is still good for the next request
    (is (equal "hello world ☃" (req (url "/text"))))))

(test no-http-redirecting-when-not-following
  (with-fixture state ()
    (handler-bind ((drakma::http-redirecting
                     (lambda (c)
                       (declare (ignore c))
                       (fail "Should not redirect"))))
      (is (eql 302 (nth-value 1 (req (url "/redirect") :redirect nil)))))))

(test any-followed-3xx-is-a-redirect-with-want-stream
  (with-fixture state ()
    (multiple-value-bind (stream code)
        (req (url "/multiple-choices") :want-stream t)
      (with-open-stream (stream stream)
        (is (eql 200 code))
        (is (equal "hello world ☃" (read-line stream)))))))

(test timeouts-before-connecting-are-connection-errors
  (with-fixture state ()
    (let ((transfer (make-instance 'util/curl/curl-request::transfer
                                   :url "http://example.com/"
                                   :connection-timeout 0
                                   :idle-timeout 0)))
      (setf (util/curl/curl-request::last-activity transfer)
            (- (get-internal-real-time) (* 2 internal-time-units-per-second)))
      (signals connection-error
        (util/curl/curl-request::check-idle transfer))
      (setf (util/curl/curl-request::connectedp transfer) t)
      (handler-case
          (progn
            (util/curl/curl-request::check-idle transfer)
            (fail "expected a timeout"))
        (connection-error ()
          (fail "should not be a connection error once connected"))
        (timeout-error ()
          (pass))))))
