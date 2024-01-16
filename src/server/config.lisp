;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/config
  (:use #:cl)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:export
   #:load-config))
(in-package :server/config)

(defvar *config-file* nil)

(defgeneric after-config-loaded (installation)
  (:method (self)
    nil)
  (:documentation "A hook that's called after the config is loaded and an installation
is generated."))

(defmethod load-config ()
  (when *config-file*
    (setf *installation*
          (with-open-file (stream *config-file*)
            (let ((*package* (find-package :cl-user)))
              (eval (read stream)))))
    (after-config-loaded *installation*)
    *installation*))
