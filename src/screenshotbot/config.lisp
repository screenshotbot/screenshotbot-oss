;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/config
    (:use #:cl
          #:alexandria)
  (:import-from #:./installation
                #:installation)
  (:export #:load-config))

(defun find-config.lisp ()
  "Search for an appropriate config.lisp file that is created by site-admin"
  (flet ((check (filename)
           (let ((filename (pathname filename)))
             (when (path:-e filename)
               (return-from find-config.lisp filename)))))
    (check "config.lisp")
    (check "~/.config/screenshotbot/config.lisp")
    nil))

(defun load-config ()
  "Load an appropriate config.lisp file if it exists"
  (let ((config.lisp (find-config.lisp)))
    (cond
      (config.lisp
       (log:info "Loading config at ~a" config.lisp)
       (let ((*package* (find-package :screenshotbot/config)))
         (load config.lisp)))
      (t
       (log:info "No config.lisp found")))))

#+screenshotbot-oss
(defmethod hunchentoot:start :before ((acceptor screenshotbot/server:acceptor))
  (load-config))
