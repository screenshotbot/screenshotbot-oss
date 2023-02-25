;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/azure/plugin
  (:use #:cl)
  (:import-from #:screenshotbot/plugin
                #:plugin)
  (:export
   #:azure-plugin))
(in-package :screenshotbot/azure/plugin)

(defclass azure-plugin (plugin)
  ())

(defun azure-plugin (&key (installation (installation)))
  (find-plugin installation 'azure-plugin))
