;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/integration-tests
  (:use #:cl
        #:fiveam)
  (:shadow #:run)
  (:import-from #:util/testing
                #:with-local-acceptor)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:with-test-user)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:alexandria
                #:remove-from-plist)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/api-key-api
                #:api-key-secret-key
                #:api-key-key)
  (:import-from #:screenshotbot/model/api-key
                #:api-key-secret))
(in-package :screenshotbot/sdk/integration-tests)

(def-suite* :integration-tests)

(defmacro with-repo (&body body)
  `(tmpdir:with-tmpdir (dir)
     (run (list "git" "clone" "https://github.com/tdrhq/fast-example" dir))
     (unwind-protect
          (let ((*default-cmd-dir* dir))
            ,@body)

       ;; On Windows, for some reason I'm unable to delete the .git
       ;; directory. Well, not *some* reason. It's just because the
       ;; directories are all marked as read-only. I don't know how
       ;; to clear that up at the moment, so I'm using the nuclear
       ;; option.
       #+(or mswindows win32)
       (run (list "attrib" "-r" (format nil "~a\\.git\\*.*" (namestring dir)) "/s")))))

(defvar *sdk* (car (asdf:output-files 'asdf:compile-op
                                       (asdf:find-component :screenshotbot.sdk/deliver
                                                            "deliver-sdk"))))
(defvar *default-cmd-dir* ".")

(defun run (cmd &rest args &key (directory *default-cmd-dir*) &allow-other-keys)
  (let* ((cmd (loop for x in cmd
                    if (pathnamep x)
                      collect (namestring x)
                    else collect x))
         (cmd (loop for x in cmd
                    collect (uiop:escape-shell-command x)))
         (cmd (str:join " " cmd))
         (cmd (format nil "cd ~a && ~a"
                      (uiop:escape-shell-command
                       (cond
                         ((pathnamep directory)
                          (namestring directory))
                         (t
                          directory)))
                      cmd)))
    (log:info "running: ~s" cmd)
    (apply #'uiop:run-program
           (append
            (cond
              ((uiop:os-windows-p)
               (list "cmd" "/c"))
              (t
               (list "bash" "-c")))
            (list
             cmd))
           (append
            (remove-from-plist args :directory)
            (list :output t
                  :error-output t)))))

(def-easy-macro with-env (name value &fn fn)
  (setf (uiop:getenv name) value)
  (log:info "Setting env ~a to ~a" name value)
  (unwind-protect
       (fn)
    (setf (uiop:getenv name) nil)))

(def-fixture state ()
  (setf (uiop:getenv "TDRHQ_IGNORE_SENTRY") "true")
  (with-installation (:globally t)
   (with-test-store (:globally t)
     (with-local-acceptor (host) ('screenshotbot/server:acceptor)
       (with-env ("SCREENSHOTBOT_API_KEY" "")
         (with-env ("SCREENSHOTBOT_API_SECRET" "")
           (with-env ("SCREENSHOTBOT_API_HOSTNAME" host)
             (with-test-user (:user user :company company :api-key api-key)
               (with-env ("SCREENSHOTBOT_API_KEY" (api-key-key api-key))
                 (with-env ("SCREENSHOTBOT_API_SECRET" (api-key-secret-key api-key))
                   (&body)))))))))))

(test preconditions
  (with-fixture state ()
    (pass)))

(test mark-failed
  (with-fixture state ()
    (with-repo
      (finishes
        (run (list *sdk*
                   "--mark-failed"
                   "--channel=foo"))))))
