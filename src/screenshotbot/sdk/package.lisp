;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot-sdk
  (:use :cl
   :alexandria
   :com.google.flag
   :anaphora)
  (:import-from :dag
   :add-commit
                :commit
   :merge-dag
                :get-commit
   :write-to-stream
   :read-from-stream)
  (:export :main))
