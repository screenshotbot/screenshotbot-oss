;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-lispworks
  (:use #:cl
        #:fiveam))
(in-package :util/tests/test-lispworks)

;; Tests for bugs in Lispworks, to make sure it's properly fixed in all
;; versions

(util/fiveam:def-suite)


(test ipv6-is-correctly-parsed
  "T1822"
  (is
   (equal
    (comm:ip-address-string (comm:parse-ipv6-address
                             "2600:1f18:153:1805:cbf0:efd:1758:b510"))
    "2600:1f18:153:1805:cbf0:efd:1758:b510"))
  (is
   (equal
    (comm:ip-address-string (comm:parse-ipv6-address
                             "2600:1f18:153:1805:cbf0:efd:1758::"))
    "2600:1f18:153:1805:cbf0:efd:1758::"))
  (is
   (equal
    (comm:ip-address-string (comm:parse-ipv6-address
                             "2600:1f18:153:1805:cbf0:efd:1758:b500"))
    "2600:1f18:153:1805:cbf0:efd:1758:b500"))
  (is
   (equal
    (comm:ip-address-string (comm:parse-ipv6-address
                             "2600:1f18:153:1805:cbf0:efd:1758:0000"))
    "2600:1f18:153:1805:cbf0:efd:1758::")))
