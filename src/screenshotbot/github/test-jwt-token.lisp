;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/github/test-jwt-token
    (:use #:cl
          #:fiveam
          #:alexandria)
  (:import-from #:./jwt-token
                #:github-create-jwt-token))

(def-suite* :screenshotbot/github/test-jwt-token)

;; this is a dummy PEM file. I would put it in the repo, but then bots
;; like GitGuardian will start pinging me. I do like their pings, so
;; let's make it less noisy.
(defvar *dummy-pem*
  "-----BEGIN RSA PRIVATE KEY-----
MIIEpQIBAAKCAQEA1G9Ca5q+4HNH8b514pvg9cDlb/dZCsYLzOBwhayW2OPtrAQU
g3NwmiV44xE9TKj2mF+ru35hZvnOosU3WfJDU83lsTWX3ZVfvZ6BkFsJogFe8hLV
4AdvwKVK0ZoYG0In3+pQ2d5AYENeyskeQrOO7rYk0KVTtECdcbKiL6aB5hCfqXS7
NURyqwMXUmJTiuNeylksd51bb4WksYDbOpYxfUOLdeygMd+8tGXsviQWXX8KkYQ2
cLHX4zOpVa3qPa3RsnTaIL+mQEHDBs3HygGeuV0fr3M6qi37MG6vVB/UmHcS6Kws
ostZs4u9DNinzc1Yl0f3hjitiYc0KrYdlccPqwIDAQABAoIBAQCmrABl4oyeF3S7
894UBI8U4dph5aRD8qyxeuptxsK3uuTv0Gez1u/i0UGujgkVS3/mfzGDMp4DKD36
sVdDR/ORHft39P+JB7iNUn5/Hx5IsGCo9yQ82DjS4hz4pLkDMf2NDg+PUHQb0t/d
pLwMQ0sCeYAa/4vT4dplqWzci/xg3v3IZcK4goJt15GX2lRpBDQg/6nzQhhDCDRe
W/qAeoUDiCwzJbaM+cJ9SYIgJQjHD9rvIXmKiumj+i6g3PcP+nRqzrpXfd3T03EF
ytEzFuRnDR2lBU4TiSXwagODzLfmAa4Fr8Ni+z8kZUhpPRf2P7e3k0/zLvlIu8CC
3syMstEZAoGBAPPehA0C9TCG1oYFSvMiQZhi4jR7xfpRZUrXHrFHNSL9XiCEwUHl
itRt+Rw9jCy5wqrz5/I4T+LwXQy9rPVzAgvOlvvkYFVVW2BCyou/BRYnFcNhO1Kw
LLuq2XiiXb2WisV+EIPOJsVUUOQt4zUJkXBMg7veQfdTE2BsEgxuZw33AoGBAN8A
ctVJt+QgbhwszgdtdpQ826X3CBu72/8RjP3K09VPGA63B03a5TPqk/MzXOpXUxLs
fGoUBqBmmVxUE4mAOhFAXsp5aQNUux3b90ppc7JCJNg/7L3/N2oAlkO28JjytdH/
XOM7a4xDclNowHqXYy8/QXCaJzZENVus1ylO2W7tAoGBAIlxN2s954JZ/D28belp
vR5tXJ/HwmS5yyTK6Plw8Hmv4oThTSoefIgNEwDfj0kFyLkgjfDt29hDL64mmHwd
PWH6JQ4CQGjXmpA+FHl+RxedH57mBdEsiYmbWMWsfLiFR+DWk+g1H5THWG/BjPQv
WFC6TuRq9zK1F46YWfO3pU0xAoGBAJcYobX8lfmfM5wpi4ui0oaMWbMxFzBbcCt6
Q9KuPCu6xK0rvGo3F7e+iHJvadRqSKJ406+4U+kYu27AlOBEnpOTzuZXrxyPq50G
rp6fpsGwaXCQl6MAqxZKwYWuDYVEZoecA97/ItbN2EfFoS0vKAgaTwexm0H8oz6z
dLHLeC3dAoGAISq6GFWVHBYK+M7zMEHruw/6pdwLdpJMCSEIzEjm882nxVhS5uQ5
PET7y5rB7N8YS86UW/DChxt8AxsJ8qvmPm1sG1oo/Wg3jwiMfSyWCEdSiVhLo/2h
JFpoRQ/PLL5bbSS5bo+qdpMFG0H0jBhSHmPem2HWjto/HpqteH4NxOI=
-----END RSA PRIVATE KEY-----
")

(test create-jwt-token-happy-path
  ;; todo: this test will break in OSS unless we specify the key
  (is (stringp (github-create-jwt-token
                :app-id "323434"
                :private-key *dummy-pem*))))
