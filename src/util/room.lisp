;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :util)

(defun room (&key (stream *standard-output*))
  "Like (room :full), but as a human readable table."
  #+(and lispworks (or linux darwin))
  (util/memory:room-table :stream stream)
  #-(and lispworks (or linux darwin))
  (let ((*standard-output* stream))
    (cl:room :full)))
