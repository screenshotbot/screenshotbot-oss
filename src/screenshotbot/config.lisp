;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/config
    (:use #:cl
          #:alexandria
          #:screenshotbot/mailer)
  (:import-from #:screenshotbot/installation
                #:oss-installation
                #:installation)
  (:import-from #:screenshotbot/github
                #:github-plugin)
  (:import-from #:screenshotbot/phabricator
                #:phabricator-plugin)
  (:import-from #:screenshotbot/mailer
                #:local-smtp-mailer
                #:noop-mailer)
  #+ (or ccl lispworks)
  (:import-from #:screenshotbot/slack
                #:slack-plugin)
  (:import-from #:screenshotbot/login/github-oauth
                #:github-oauth-provider)
  (:import-from #:screenshotbot/login/google-oauth
                #:google-oauth-provider)
  (:import-from #:screenshotbot/login/oidc
                #:oidc-provider)
  (:export #:load-config))
(in-package :screenshotbot/config)

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
       (log:info "No config.lisp found")
       (setf (installation) (make-instance 'oss-installation))))))

;;;; I suppose I'm misusing the screenshotbot/config package for bot
;;;; this file and for the package the config.lisp is loaded from. In
;;;; any case, I didn't want to pollute the package so that's why the
;;;; following symbols are not imported.

(screenshotbot/admin/core:defadminhandler (reload-config-page :uri "/admin/reload-config") ()
  (hex:safe-redirect (nibble:nibble (:once t)
                       (core/ui/simple-card-page:confirmation-page
                        :yes (nibble:nibble (:once t)
                               (load-config)
                               (hex:safe-redirect "/admin"))
                        :no "/admin"
                        "Reload the config.lisp?"))))

(screenshotbot/admin/core:register-admin-menu "Reload config.lisp" 'reload-config-page)
