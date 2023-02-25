;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/azure/promoter
  (:use #:cl
        #:screenshotbot/azure/plugin)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:valid-repo?
                #:abstract-pr-promoter)
  (:import-from #:screenshotbot/promote-api
                #:plugin-promoter)
  (:import-from #:screenshotbot/azure/plugin
                #:azure-plugin))
(in-package :screenshotbot/azure/promoter)

(defclass azure-promoter (abstract-pr-promoter)
  ())

(defmethod plugin-promoter ((self azure-plugin))
  (make-instance 'azure-promoter))

(defmethod valid-repo? ((self azure-promoter) (repo azure-git-repo))
  t)
