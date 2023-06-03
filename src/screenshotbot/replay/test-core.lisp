;;;; -*- coding: utf-8 -*-
;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/test-core
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/replay/core
                #:http-header-value
                #:http-header-name
                #:asset-response-headers
                #:request-counter
                #:root-urls
                #:root-assets
                #:remove-unwanted-headers
                #:+empty-headers+
                #:%lru-cache
                #:*cache*
                #:fix-malformed-url
                #:process-node
                #:http-cache-dir
                #:context
                #:remote-response
                #:guess-external-format
                #:load-url-into
                #:url
                #:assets
                #:snapshot
                #:should-rewrite-url-p
                #:read-srcset
                #:push-asset
                #:rewrite-css-urls
                #:http-get)
  (:import-from #:util/lru-cache
                #:lru-cache)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:has-item
                #:contains)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:screenshotbot/api/model
                #:decode-json
                #:encode-json)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/replay/test-core)

(util/fiveam:def-suite)

(def-fixture state ()
  (tmpdir:with-tmpdir (tmpdir)
    (let ((*cache* (make-instance 'lru-cache
                                   :dir tmpdir))
          (context (make-instance 'context)))
      (cl-mock:with-mocks ()
       (&body)))))


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
  (with-fixture state ()
   (tmpdir:with-tmpdir (tmpdir)
     (cl-mock:if-called 'util/request:http-request
                        (lambda (url &rest args)
                          (values
                           (flexi-streams:make-in-memory-input-stream
                            #())
                           200
                           +empty-headers+)))

     (let* ((snapshot (make-instance 'snapshot :tmpdir tmpdir))
            (rand (random 10000000000))
            (font (format nil "https://screenshotbot.io/assets/fonts/metropolis/Metropolis-Bold-arnold.otf?id=~a" rand))
            (html (format nil "https://screenshotbot.io/?id=~a" rand)))

       (push-asset context snapshot (quri:uri html) nil)
       (is (equal 1 (length (assets snapshot))))
       (push-asset context snapshot (quri:uri font)  t)
       (is (equal 2 (length (assets snapshot))))
       (is (equal font
                  (url (car (Assets snapshot)))))
       (push-asset context snapshot (quri:uri font)  t)

       (is (equal 2 (length (assets snapshot))))
       (push-asset context snapshot (quri:uri html) nil)
       (is (equal 2 (length (assets snapshot))))
       (push-asset context snapshot (quri:uri font)  t)
       (is (equal 2 (length (assets snapshot))))))))

(test happy-path-fetch-toplevel-only-once
  (with-fixture state ()
   (tmpdir:with-tmpdir (tmpdir)
     (cl-mock:if-called 'util/request:http-request
                        (lambda (url &rest args)
                          (values
                           (flexi-streams:make-in-memory-input-stream
                            (flexi-streams:string-to-octets
                             "<html><body></body></html>"))
                           200
                           +empty-headers+)))

     (let ((snapshot (make-instance 'snapshot :tmpdir tmpdir)))
       ;; Just verifying that on Windows, we don't keep any stale file descriptors around
       (finishes (load-url-into context snapshot (quri:uri "https://screenshotbot.io/") tmpdir))
       (is (eql 1 (length (root-urls snapshot))))
       (assert-that (car (root-urls snapshot))
                    (is-equal-to
                     "https://screenshotbot.io/"))
       (assert-that (root-assets snapshot)
                    (has-length 1))))))

(test can-encode-snapshot
  (with-fixture state ()
   (tmpdir:with-tmpdir (tmpdir)
     (cl-mock:if-called 'util/request:http-request
                        (lambda (url &rest args)
                          (values
                           (flexi-streams:make-in-memory-input-stream
                            (flexi-streams:string-to-octets
                             "<html><body></body></html>"))
                           200
                           +empty-headers+)))
     (let ((snapshot (make-instance 'snapshot :tmpdir tmpdir)))
       (load-url-into context snapshot (quri:uri "https://screenshotbot.io/") tmpdir)
       (assert-that (assets snapshot)
                    (has-length 1))
       (flet ((check-snapshot (snapshot)
                (is (eql 0 (request-counter snapshot)))
                (let* ((asset (car (assets snapshot)))
                       (header (find
                                "content-type"
                                (asset-response-headers asset)
                                :key #'http-header-name
                                :test #'string-equal)))
                  (assert-that (mapcar #'http-header-name
                                       (asset-response-headers asset))
                               (has-item "content-type"))
                  (is-true header)
                  (is (equal "text/html; charset=UTF-8" (http-header-value header))))))
         (check-snapshot snapshot)
         (let* ((encoded
                  (encode-json snapshot))
                (snapshot (decode-json encoded 'snapshot)))
           #+nil
           (check-snapshot snapshot)))))))

(test two-urls-with-same-content
  (with-fixture state ()
   (tmpdir:with-tmpdir (tmpdir)
     (cl-mock:if-called 'util/request:http-request
                        (lambda (url &rest args)
                          (values
                           (flexi-streams:make-in-memory-input-stream
                            (flexi-streams:string-to-octets
                             "<html><body></body></html>"))
                           200
                           `(("x-foobar" . 1)))))

     (let ((snapshot (make-instance 'snapshot :tmpdir tmpdir)))
       ;; Just verifying that on Windows, we don't keep any stale file descriptors around
       (load-url-into context snapshot (quri:uri "https://screenshotbot.io/one") tmpdir)
       (load-url-into context snapshot (quri:uri "https://screenshotbot.io/two") tmpdir)

       (assert-that (root-assets snapshot)
                    (has-length 2))
       (assert-that (root-urls snapshot)
                    (has-length 2))
       (assert-that (sort (mapcar #'url (root-assets snapshot)) #'string<)
                    (contains
                     "https://screenshotbot.io/one"
                     "https://screenshotbot.io/two"))))))

(test identical-content-on-two-pages
  (with-fixture state ()
   (tmpdir:with-tmpdir (tmpdir)
     (cl-mock:if-called 'util/request:http-request
                        (lambda (url &rest args)
                          (values
                           (flexi-streams:make-in-memory-input-stream
                            (flexi-streams:string-to-octets
                             "<html><body></body></html>"))
                           200
                           +empty-headers+)))

     (let ((snapshot (make-instance 'snapshot :tmpdir tmpdir)))
       (finishes (load-url-into context snapshot (quri:uri "https://screenshotbot.io/") tmpdir))
       (finishes (load-url-into context snapshot (quri:uri "https://screenshotbot.io/foobar") tmpdir))
       (is (eql 2 (length (root-urls snapshot))))))))

(test identical-content-on-two-pages-with-different-actual-url
  (with-fixture state ()
   (tmpdir:with-tmpdir (tmpdir)
     (cl-mock:if-called 'util/request:http-request
                        (lambda (url &rest args)
                          (values
                           (flexi-streams:make-in-memory-input-stream
                            (flexi-streams:string-to-octets
                             "<html><body></body></html>"))
                           200
                           +empty-headers+)))

     (let ((snapshot (make-instance 'snapshot :tmpdir tmpdir)))
       (finishes (load-url-into context snapshot (quri:uri "https://screenshotbot.io/deadbeaf/") tmpdir :actual-url "foobar-1"))
       (finishes (load-url-into context snapshot (quri:uri "https://screenshotbot.io/deadbeef/") tmpdir
                                :actual-url "foobar-2"))
       (is (eql 2 (length (root-urls snapshot))))))))

(test happy-path-fetch-toplevel
  (with-fixture state ()
   (tmpdir:with-tmpdir (tmpdir)
     (cl-mock:if-called 'util/request:http-request
                        (lambda (url &rest args)
                          (values
                           (flexi-streams:make-in-memory-input-stream
                            (flexi-streams:string-to-octets
                             "<html><body></body></html>"))
                           200
                           +empty-headers+)))

     (let ((snapshot (make-instance 'snapshot :tmpdir tmpdir)))
       (load-url-into context snapshot (quri:uri "https://screenshotbot.io/") tmpdir))
          (let ((snapshot (make-instance 'snapshot :tmpdir tmpdir)))
            (load-url-into context snapshot "https://screenshotbot.io/" tmpdir)
            (pass)))))

(test adds-screenshotbot-css
  (with-fixture state ()
    (let ((html (plump:parse "<html><body>hello</body></html>")))
      (process-node (make-instance 'context)
                    html
                    (make-instance 'snapshot)
                    "https://www.google.com")
      (is (equal "<html><body class=\" screenshotbot\">hello</body></html>"
                 (with-output-to-string (s)
                  (plump:serialize html s))))
      (pass))))

(test utf-8
  (with-fixture state ()
   (tmpdir:with-tmpdir (tmpdir)
     (cl-mock:if-called 'util/request:http-request
                        (lambda (url &rest args)
                          (values
                           (flexi-streams:make-in-memory-input-stream
                            (flexi-streams:string-to-octets
                             "<html><body>©</body></html>"
                             :external-format :utf-8))
                           200
                           `((:content-type . "text/html; charset=utf-8")))))

     (with-open-stream (content (http-get "https://example.com" :force-string t
                                                                :force-binary nil))
       (is (equal "<html><body>©</body></html>" (uiop:slurp-input-stream :string content))))
     (with-open-stream (content (http-get "https://example.com" :force-string t
                                                                :force-binary nil))
       (is (equal "<html><body>©</body></html>" (uiop:slurp-input-stream :string content)))))))

(test guess-external-format
  (uiop:with-temporary-file (:pathname p)
   (flet ((make-info (content-type)
            (let ((map `((:content-type . ,content-type))))
              (make-instance 'remote-response
                             :headers map))))
     (is (equal :utf-8
                (guess-external-format (make-info "text/html; charset=utf-8") p)))
     (is (equal :utf-8
                (guess-external-format (make-info "text/html; charset=UTF-8") p)))
     (is (equal :utf-8
                (guess-external-format (make-info "text/html; charset='utf-8' ") p))))))

(test guess-external-format-from-content-utf-8
  (uiop:with-temporary-file (:pathname p :stream s)
   (flet ((make-info (content-type)
            (let ((map `((:content-type . ,content-type))))
              (make-instance 'remote-response
                             :headers map))))
     (write-string "<html><head><meta charset='utf-8'></head></html>"
                   s)
     (finish-output s)
     (is (equal :utf-8
                (guess-external-format (make-info "text/html") p))))))

(test guess-external-format-from-content-latin-1
  (uiop:with-temporary-file (:pathname p :stream s)
   (flet ((make-info (content-type)
            (let ((map `((:content-type . ,content-type))))
              (make-instance 'remote-response
                             :headers map))))
     (write-string "<html><head></head></html>"
                   s)
     (finish-output s)
     (is (equal :latin-1
                (guess-external-format (make-info "text/html") p))))))

(test http-cache-dir
  (with-fixture state ()
    (tmpdir:with-tmpdir (util:*object-store*)
      (let ((*cache* nil))
        (is (path:-d (path:catdir (util/lru-cache::dir (%lru-cache)))))))))

(test fix-malformed-url
  (is (equal
       "https://www.rollins.edu/academic-advising/images/Tres%20Loch.jpg"
       (fix-malformed-url
        "https://www.rollins.edu/academic-advising/images/Tres Loch.jpg"))))

(test http-get-ignores-invalid-url
  (with-fixture state ()
    (multiple-value-bind (stream ret) (http-get "http://127.0.0.1/????invalid")
      (close stream)
      (is (equal
           500
           ret)))))

(test remove-unwanted-headers
  (is (equal
       `((:foo . "bar"))
       (remove-unwanted-headers
        `((:x-foo-bar . "bleh")
          (:foo . "bar")))))
  (is (equal
       `((:foo . "bar"))
       (remove-unwanted-headers
        `((:alt-svc . "bleh")
          (:foo . "bar")))))
    (is (equal
       `((:foo . "bar"))
       (remove-unwanted-headers
        `((:content-security-policy . "bleh")
          (:foo . "bar"))))))
