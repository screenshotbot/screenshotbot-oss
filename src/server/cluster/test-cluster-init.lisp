;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/cluster/test-cluster-init
  (:use #:cl
        #:fiveam)
  (:import-from #:server/cluster/cluster-init
                #:map-to-ips
                #:best-peer
                #:map-to-peers))
(in-package :server/cluster/test-cluster-init)


(util/fiveam:def-suite)

(test map-to-peers
  (let ((map (fset:with
              (fset:empty-map)
              "Instance1" "1.1.1.1:7070:0")))
    (is (equal "1.1.1.1:7070:0" (map-to-peers map)))
    (fset:includef map "Instance2" "1.1.1.2:7070:0")
    (is (equal "1.1.1.1:7070:0,1.1.1.2:7070:0" (map-to-peers map)))))

(test best-peer
  (let ((peers (fset:with
                (fset:with
                 (fset:with
                  (fset:empty-map)
                  "Instance1" "1.1.1.1:7070:0:0")
                 "Instance2" "1.1.1.2:7070:0:0")
                "Instance3" "1.1.1.3:7070:0:0")))
    (is (equal "1.1.1.2:7070:0:0"
               (best-peer peers "Instance1")))
    (is (equal "1.1.1.1:7070:0:0"
               (best-peer peers "Instance2")))
    (is (equal "1.1.1.1:7070:0:0"
               (best-peer peers "Instance3")))))

(test map-to-ips
  (let ((peers (fset:with
                (fset:with
                 (fset:with
                  (fset:empty-map)
                  "Instance1" "1.1.1.1:7070:0:0")
                 "Instance2" "1.1.1.2:7070:0:0")
                "Instance3" "1.1.1.3:7070:0:0")))
    (is
     (equal
      (list "1.1.1.1" "1.1.1.2" "1.1.1.3")
      (map-to-ips peers)))))

