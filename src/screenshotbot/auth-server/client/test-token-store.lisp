;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth-server/client/test-token-store
  (:use #:cl
        #:fiveam)
  (:import-from #:auth-server/client/token-store
                #:access-token-string
                #:load-tokens
                #:refresh-token-string
                #:save-tokens
                #:token-expires-at
                #:token-scope
                #:token-usable-p
                #:tokens-from-response))
(in-package :auth-server/client/test-token-store)

(util/fiveam:def-suite)

(defparameter +host+ "https://staging.screenshotbot.io")

(defparameter +response+
  '(("access_token" . "the-access-token")
    ("refresh_token" . "the-refresh-token")
    ("token_type" . "Bearer")
    ("expires_in" . 3600)
    ("scope" . "api:read")))

(def-fixture state ()
  (tmpdir:with-tmpdir (dir)
    (let ((token-file (merge-pathnames "oauth-token.json" dir)))
      (&body))))

(test round-trips-through-the-cache-file
  (with-fixture state ()
    (save-tokens token-file +host+ +response+)
    (let ((loaded (load-tokens token-file +host+)))
      (is-true loaded)
      (is (equal "the-access-token" (access-token-string loaded)))
      (is (equal "the-refresh-token" (refresh-token-string loaded)))
      (is (equal "api:read" (token-scope loaded)))
      (is-true (token-usable-p loaded)))))

(test tokens-are-not-shared-across-installations
  "A staging token must never be presented to production."
  (with-fixture state ()
    (save-tokens token-file +host+ +response+)
    (is-false (load-tokens token-file "https://screenshotbot.io"))
    (is-true (load-tokens token-file +host+))))

(test a-missing-or-corrupt-cache-is-a-miss-not-an-error
  "The caller's fallback is to sign in again, which is right in every one
of these cases."
  (with-fixture state ()
    (is-false (load-tokens token-file +host+))
    (with-open-file (stream token-file :direction :output :if-exists :supersede)
      (write-string "this is not json" stream))
    (is-false (load-tokens token-file +host+))
    (with-open-file (stream token-file :direction :output :if-exists :supersede)
      (write-string "{}" stream))
    (is-false (load-tokens token-file +host+))))

(test expires-in-is-stored-as-an-absolute-time
  "A relative expiry is meaningless once it has been sitting in a file."
  (with-fixture state ()
    (let ((before (get-universal-time)))
      (save-tokens token-file +host+ +response+)
      (let ((loaded (load-tokens token-file +host+)))
        (is (<= (+ before 3600) (token-expires-at loaded)))
        (is (<= (token-expires-at loaded) (+ (get-universal-time) 3600)))))))

(test an-expired-token-is-not-usable
  (with-fixture state ()
    (let ((tokens (tokens-from-response
                   (append '(("expires_in" . -1)) +response+)
                   +host+)))
      (is-false (token-usable-p tokens)))))

(test a-token-about-to-expire-is-not-usable
  "It would otherwise expire mid-request."
  (with-fixture state ()
    (let ((tokens (tokens-from-response
                   (append '(("expires_in" . 30)) +response+)
                   +host+)))
      (is-false (token-usable-p tokens)))))

(test saving-twice-replaces-rather-than-appends
  (with-fixture state ()
    (save-tokens token-file +host+ +response+)
    (save-tokens token-file +host+
                 (append '(("access_token" . "second-token")) +response+))
    (is (equal "second-token"
               (access-token-string (load-tokens token-file +host+))))))

(defun file-mode (path)
  "PATH's permission bits as an octal string.

BSD stat and GNU stat share a name and nothing else."
  (str:trim
   (uiop:run-program (if (uiop:os-macosx-p)
                         (list "stat" "-f" "%Lp" (namestring path))
                         (list "stat" "-c" "%a" (namestring path)))
                     :output :string)))

#-mswindows
(test the-cache-file-is-not-readable-by-anyone-else
  "It holds a bearer credential."
  (with-fixture state ()
    (save-tokens token-file +host+ +response+)
    (is (equal "600" (file-mode token-file)))))
