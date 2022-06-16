;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;; for non Lispworks only

(defpackage :util/sha256
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:sha256-file))
(in-package :util/sha256)

(defun sha256-file (file)
  (ironclad:digest-file :sha256 file))
