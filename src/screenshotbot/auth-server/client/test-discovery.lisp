;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth-server/client/test-discovery
  (:use #:cl
        #:fiveam)
  (:import-from #:auth-server/client/discovery
                #:authorization-endpoint
                #:conventional-metadata
                #:device-authorization-endpoint
                #:discover
                #:repoint
                #:revocation-endpoint
                #:token-endpoint))
(in-package :auth-server/client/test-discovery)

(util/fiveam:def-suite)

(test conventional-endpoints-hang-off-the-host
  (let ((metadata (conventional-metadata "https://staging.screenshotbot.io")))
    (is (equal "https://staging.screenshotbot.io/oauth/authorize"
               (authorization-endpoint metadata)))
    (is (equal "https://staging.screenshotbot.io/oauth/token"
               (token-endpoint metadata)))
    (is (equal "https://staging.screenshotbot.io/oauth/device/code"
               (device-authorization-endpoint metadata)))
    (is (equal "https://staging.screenshotbot.io/oauth/revoke"
               (revocation-endpoint metadata)))))

(test a-trailing-slash-on-the-host-does-not-double-up
  (is (equal "https://example.com/oauth/token"
             (token-endpoint (conventional-metadata "https://example.com/")))))

(test a-host-with-an-explicit-port-is-preserved
  (is (equal "http://localhost:4095/oauth/token"
             (token-endpoint (conventional-metadata "http://localhost:4095")))))

(test repoint-keeps-the-path-and-takes-the-host
  "A dev server advertises its canonical issuer, which is usually not the
address we reached it on."
  (is (equal "http://localhost:4095/oauth/token"
             (repoint "https://screenshotbot.io/oauth/token"
                      "http://localhost:4095")))
  (is (equal "https://staging.screenshotbot.io/oauth/authorize"
             (repoint "https://screenshotbot.io/oauth/authorize"
                      "https://staging.screenshotbot.io"))))

(test repoint-never-follows-an-advertised-host
  "Otherwise a misconfigured -- or hostile -- issuer could send our
credentials somewhere we never asked to talk to."
  (let ((repointed (repoint "https://evil.example.com/oauth/token"
                            "https://staging.screenshotbot.io")))
    (is (equal "https://staging.screenshotbot.io/oauth/token" repointed))
    (is-false (str:containsp "evil.example.com" repointed))))

(test discovery-can-be-skipped-entirely
  (let ((metadata (discover "http://localhost:4095" :use-discovery nil)))
    (is (equal "http://localhost:4095/oauth/token" (token-endpoint metadata)))))

(test an-unreachable-server-falls-back-to-the-standard-paths
  "Discovery is a convenience; a server that doesn't publish metadata, or
isn't up yet, still has to work."
  (let ((metadata (discover "http://127.0.0.1:1" :use-discovery t)))
    (is (equal "http://127.0.0.1:1/oauth/token" (token-endpoint metadata)))))
