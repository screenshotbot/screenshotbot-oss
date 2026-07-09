;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/cluster/test-cluster-init
  (:use #:cl
        #:fiveam)
  (:import-from #:server/cluster/cluster-init
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

