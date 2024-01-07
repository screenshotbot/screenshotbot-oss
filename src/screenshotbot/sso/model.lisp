;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sso/model
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:fake-sso-auth-provider))
(in-package :screenshotbot/sso/model)

(defclass fake-sso-auth-provider (store-object)
  ()
  (:metaclass persistent-class))
