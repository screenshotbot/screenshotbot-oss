;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop/package:define-package :screenshotbot/plugin
    (:use #:cl #:alexandria)
  (:export #:plugin
           #:plugin-parse-repo))
(in-package :screenshotbot/plugin)

(defclass plugin ()
  ())

(defgeneric plugin-parse-repo (plugin company repo-str)
  (:documentation "Parses a repo string and generates a corresponding repo
  object. Return NIL if it doesn't look like a repo that this plugin
  can parse."))

(defmethod plugin-parse-repo (plugin company repo-str)
  nil)
