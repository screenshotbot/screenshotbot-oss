;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/microsoft-entra
  (:use #:cl)
  (:import-from #:screenshotbot/login/oidc
                #:oidc-provider)
  (:import-from #:auth/login/cached-avatar
                #:cached-avatar-provider)
  (:export
   #:microsoft-entra-provider))
(in-package :screenshotbot/login/microsoft-entra)

(defclass microsoft-entra-provider (oidc-provider
                                    cached-avatar-provider)
  ()
  (:documentation "A provider for Microsoft Entra (Azure). In particular the default
OIDC provider doesn't handle profile pictures correctly for Entra."))


