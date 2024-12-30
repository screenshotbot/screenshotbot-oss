;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :nibble
  (:use :cl
   :alexandria)
  (:export #:nibble
           #:nibble-full-url
           #:nibble-url
           #:get-nibble
           #:render-nibble
           #:nibble-id
           #:defnibble
           #:nibble-current-user
           #:nibble-acceptor-mixin
           #:allow-user-change
           #:nibble-funcall))
