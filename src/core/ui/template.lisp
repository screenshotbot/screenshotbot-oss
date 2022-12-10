;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/ui/template
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:*app-template*
   #:app-template))
(in-package :core/ui/template)

(named-readtables:in-readtable markup:syntax)

(defclass base-template ()
  ())

(defvar *app-template* (make-instance 'base-template))

(defgeneric render-template (template children &key &allow-other-keys)
  (:method ((self base-template) children &key &allow-other-keys)
    <html>
      <body>
        ,@children
      </body>
    </html>))

(markup:deftag app-template (children &key title stripe)
  (render-template
   *app-template*
   children
   :title title
   :stripe stripe))
