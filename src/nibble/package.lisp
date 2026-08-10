;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :nibble
  (:use :cl
   :alexandria)
  (:import-from #:bknr.indices
                #:indexed-class
                #:base-indexed-object)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:util/store/fset-index
                #:fset-set-index
                #:fset-unique-index)
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
           #:nibble-funcall
           #:expired-nibble-src
           #:expired-nibble))
