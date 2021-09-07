;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/slack/plugin
  (:use #:cl #:alexandria)
  (:import-from #:screenshotbot/installation
                #:plugin
                #:plugins
                #:find-plugin
                #:installation)
  (:export
   #:slack-plugin
   #:client-id
   #:client-secret))
(in-package :screenshotbot/slack/plugin)

(defclass slack-plugin (plugin)
  ((client-id :initarg :client-id
              :initform (error "Need to provide :client-id for slack-plugin")
              :accessor client-id)
   (client-secret :initarg :client-secret
                  :initform (error "Need to provide :client-secret for slack-plugin")
                  :accessor client-secret)))

(defun slack-plugin (&key (installation (installation)))
  (find-plugin installation 'slack-plugin))
