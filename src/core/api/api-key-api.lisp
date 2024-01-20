;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; This package is no longer needed, technically. It could be safe to
;; delete this, and replace it all with model/api-key. But, it WILL
;; require a server restart, so beware.
(uiop/package:define-package :screenshotbot/api-key-api
  (:use #:cl #:alexandria)
  (:nicknames :core/api/api-key-api)
  (:export
   #:api-key-key
   #:api-key
   #:api-key-secret-key
   #:delete-api-key))
(in-package :screenshotbot/api-key-api)

(defgeneric api-key-key (api-key))
(defgeneric api-key-secret-key (api-key))
(defgeneric delete-api-key (api-key))
