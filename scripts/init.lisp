;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;; This file is loaded after the image is restarted

(ql:quickload :deadbeef)
(deadbeef:register-external "https://github.com/tdrhq/stripe"
                            "6b91ee9bcbffe81f887a0edddd1b182951cd02cf")
(deadbeef:prepare-externals "build/deadbeef/")
