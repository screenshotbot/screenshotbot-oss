;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;; for non Lispworks only

(defpackage :util/digests
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:sha256-file
   #:md5-file))
(in-package :util/digests)

(defun sha256-file (file)
  (ironclad:digest-file :sha256 file))

(defun md5-file (file)
  (md5:md5sum-file file))
