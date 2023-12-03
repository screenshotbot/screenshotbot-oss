;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/cli
  (:use #:cl)
  (:import-from #:clingon
                #:make-option)
  (:import-from #:server
                #:init-multi-acceptor
                #:*multi-acceptor*
                #:*slynk-loopback-interface*
                #:%run
                #:%verify
                #:*slynk-port*
                #:*start-slynk*
                #:with-common-setup)
  (:import-from #:util/health-check
                #:run-health-checks)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:util/misc
                #:with-global-binding)
  (:import-from #:util/store
                #:*object-store*)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:server/config
                #:*config-file*)
  (:import-from #:util/threading
                #:make-thread)
  #+lispworks
  (:import-from #:hunchentoot-extensions/existing-socket
                #:existing-socket)
  (:export
   #:main))
(in-package :server/cli)

(def-easy-macro with-store (cmd &fn fn)
  (with-global-binding ((*object-store* (serapeum:ensure-suffix (clingon:getopt cmd :store) "/"))
                        (*start-slynk* (clingon:getopt cmd :start-slynk))
                        (*slynk-loopback-interface* (clingon:getopt cmd :slynk-loopback-interface))
                        (*slynk-port* (format nil "~a" (clingon:getopt cmd :slynk-port))))
    (fn)))

(defun self-test/command (&key enable-store jvm)
  (clingon:make-command
   :name "self-test"
   :handler (lambda (cmd)
              (with-store (cmd)
               (with-common-setup (:enable-store enable-store :jvm jvm)
                 (run-health-checks))))
   :options (list* (common-options))))

(def-easy-macro with-screenshotbot-config (cmd &fn fn)
  (cond
    ((find-package :screenshotbot/pro/installation)
     (unless (clingon:getopt cmd :config)
       (error "Must provide a --config file"))
     (with-global-binding ((*config-file* (clingon:getopt cmd :config)))
       #-screenshotbotoss
       (uiop:call-function "screenshotbot/pro/installation:init-pro-installation")
       (fn)))
    (t
     (when (clingon:getopt cmd :config)
       (error "--config is not supported here"))
     (fn))))

(def-easy-macro with-run-or-verify-setup (cmd &key enable-store jvm &fn fn)
  (process-common-options cmd)
  (with-screenshotbot-config (cmd)
    (with-store (cmd)
      (when-let ((secrets (clingon:getopt cmd :secrets)))
        (log:info "Loading secrets from ~a" secrets)
        (setf util/phabricator/passphrase:*secret-file* secrets)
        (util/phabricator/passphrase:reload-secrets))
      (with-common-setup (:enable-store enable-store :jvm jvm)
        (fn)))))

(defun process-common-options (cmd)
  (when (clingon:getopt cmd :verbose)
    (log:config :debug)))

(defun verify/command (&key enable-store jvm)
  (clingon:make-command
   :name "verify"
   :handler (lambda (cmd)
              (with-run-or-verify-setup (cmd :enable-store enable-store :jvm jvm)
                (%verify :profile-store (clingon:getopt cmd :profile))))
   :options (list*
             (make-option
              :flag
              :description "Whether to run the profiler when verifying the store"
              :initial-value nil
              :long-name "profile"
              :key :profile)
             (common-options))))

(defvar *signal-lock* (bt:make-lock))

#+lispworks
(defun sigusr1-handler (&rest x)
  (declare (ignore x))
  (make-thread
   (lambda ()
     (let ((stream (sys:make-stderr-stream)))
       (bt:with-lock-held (*signal-lock*)
         (log:info "SIGUSR1: Started")
         (format stream "SIGUSR1: Started~%")
         (finish-output stream)
         (asdf:load-system :screenshotbot.pro)
         (util:validate-indices)
         (format stream "SIGUSR1: Success ~a.~%"
                 (uiop:read-file-string "release_timestamp"))
         (finish-output stream))))
   :name "SIGUSR1-thread"))

(defun apply-socket (acceptor socket)
  #+lispworks
  (when socket
    (setf (existing-socket acceptor) socket))
  acceptor)

(defun run/command (&key enable-store jvm acceptor)
  (clingon:make-command
   :name "run"
   :handler (lambda (cmd)
              (with-run-or-verify-setup (cmd :enable-store enable-store :jvm jvm)
                #+lispworks
                (sys:set-signal-handler 10
                                        'sigusr1-handler)
                (%run :enable-store enable-store
                      :acceptor
                      (apply-socket
                       (cond
                         (acceptor
                          acceptor)
                         ((and
                           nil ;; Disable for now.
                           (clingon:getopt cmd :only-screenshotbot-p))
                          (symbol-value
                           (uiop:find-symbol* :*acceptor* :screenshotbot/server)))
                         (t
                          (init-multi-acceptor :port (clingon:getopt cmd :port))
                          *multi-acceptor*))
                       (clingon:getopt cmd :socket))
                      :port (clingon:getopt cmd :port)
                      :shell nil)))
   :options (list*
             (make-option
              :integer
              :description "HTTP access port"
              :long-name "port"
              :initial-value 4001
              :key :port)
             (make-option
              :integer
              :description "HTTP listening socket, usually set up my systemd"
              :long-name "socket"
              :key :socket)
             (make-option
              :flag
              :description "Only load the screenshotbot acceptor, instead of the multiacceptor"
              :initial-value nil
              :long-name "only-screenshotbot"
              :key :only-screenshotbot-p)
             (common-options))))

(defun save-passphrases/command ()
  (clingon:make-command
   :name "save-passphrases"
   :handler (lambda (cmd)
              (let ((output (clingon:getopt cmd :output)))
                (uiop:call-function
                 "util/phabricator/passphrase::save-passphrases"
                 output)))
   :options (list
             (make-option
              :filepath
              :description "Where to save the passphrases"
              :long-name "output"
              :key :output))))

(defun main/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defun common-options ()
  (list
   (make-option
    :filepath
    :description "The object store location"
    :short-name #\s
    :long-name "store"
    :key :store)
   (make-option
    :filepath
    :description "Path to secrets file. Ignore on OSS."
    :long-name "secrets"
    :key :secrets)
   (make-option
    :flag
    :description "Whether to start slynk"
    :long-name "start-slynk"
    :initial-value :true
    :key :start-slynk)
   (make-option
    :string
    :description "The IP address to listen on for slynk"
    :long-name "slynk-loopback-interface"
    :initial-value "127.0.0.1"
    :key :slynk-loopback-interface)
   (make-option
    :filepath
    :description "Config file to use (not for OSS)"
    :long-name "config"
    :key :config)
   (make-option
    :flag
    :description "whether to enable verbose logs"
    :long-name "verbose"
    :key :verbose)
   (make-option
    :integer
    :description "the port to start slynk on"
    :long-name "slynk-port"
    :initial-value 4005
    :key :slynk-port)))


(defun main/command (&key enable-store jvm acceptor)
  (clingon:make-command :name "App Server"
                        :handler #'main/handler
                        :sub-commands
                        (remove-if #'null
                         (list
                          (self-test/command :enable-store enable-store
                                             :jvm jvm)
                          (verify/command :enable-store enable-store
                                          :jvm jvm)
                          #-screenshotbot-oss
                          (when (find-package :screenshotbot/pro/installation)
                            (uiop:call-function
                             "screenshotbot/pro/installation:gen-config/command"))
                          (save-passphrases/command)
                          (run/command :enable-store enable-store
                                       :jvm jvm
                                       :acceptor acceptor)
                          #+lispworks
                          (server/eval:eval/command)))))

(defun legacy-mode-p (args)
  (and (second args)
       (eql #\- (elt (second args) 0))))

(defun main (&key (jvm t) acceptor (enable-store t))
  (handler-bind ((error (lambda (e)
                          (format t "Got error during init process: ~a~%" e)
                          #+lispworks
                          (dbg:output-backtrace :bug-form t)
                          (uiop:quit 1))))
   (cond
     ((legacy-mode-p sys:*line-arguments-list*)
      (warn "Using legacy mode for command line parsing")
      (server:main :jvm jvm :acceptor acceptor :enable-store enable-store))
     (t
      (let ((args #-lispworks (cdr (uiop:raw-command-line-arguments))
                  #+lispworks (cdr sys:*line-arguments-list*)))
        (let ((app (main/command :jvm jvm :enable-store enable-store
                                 :acceptor acceptor)))
          (clingon:run app args)
          (uiop:quit 0)))))))
