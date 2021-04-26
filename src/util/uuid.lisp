;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :util)

(defun safe-uuid ()
  (uuid:print-bytes nil (uuid:make-v4-uuid)))

(defun make-secret-code ()
  (base32:bytes-to-base32 (secure-random:bytes 32
                                               secure-random:*generator*)))
