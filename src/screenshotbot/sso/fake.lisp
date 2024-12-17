;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sso/fake
  (:use #:cl
        #:screenshotbot/sso/model)
  (:import-from #:nibble
                #:nibble)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/sso/fake)

(markup:enable-reader)

