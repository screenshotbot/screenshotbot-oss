;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/cli/sentry
  (:use #:cl)
  (:import-from #:util/threading
                #:with-tags
                #:maybe-log-sentry
                #:*warning-count*
                #:with-extras
                #:*extras*
                #:funcall-with-sentry-logs)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:export
   #:default-sentry-output-stream
   #:with-cli-sentry))
(in-package :core/cli/sentry)

(defun default-sentry-output-stream ()
  #+lispworks
  (system:make-stderr-stream)
  #-lispworks
  *standard-output*)

(def-easy-macro with-cli-sentry (&key verbose
                                      (dry-run nil)
                                      (stream
                                       (default-sentry-output-stream))
                                      (on-error (lambda ()
                                                  (uiop:quit 1)))
                                      &fn fn)
  (with-extras (#+lispworks
                ("cmd-line-trimmed"
                 (mapcar
                  #'trim-arg
                  sys:*line-arguments-list*))
                ("hostname" (uiop:hostname))
                #+lispworks
                ("openssl-version" (comm:openssl-version)))
   (let ((error-handler (lambda (e)
                          (format stream "~%~a~%~%" e)
                          #+lispworks
                          (dbg:output-backtrace (if verbose :bug-form :brief)
                                                :stream stream)
                          #-lispworks
                          (trivial-backtrace:print-backtrace e stream)
                          (unless dry-run
                            #-screenshotbot-oss
                            (util/threading:log-sentry e))
                          (funcall on-error))))
     (let ((*warning-count* 0))
       (handler-bind (#+lispworks
                      (error error-handler))
         ;; We put the warning handler inside here, so that if an
         ;; error happens in the warning handler, we can log that.
         (handler-bind (#+lispworks
                        (warning #'maybe-log-sentry))
           (funcall fn)))))))
