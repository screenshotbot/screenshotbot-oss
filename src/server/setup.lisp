;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage #:server
  (:use #:cl
        #:bknr.datastore
        #:server/interrupts)
  (:import-from #:util/health-check
                #:run-health-checks)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:lparallel.kernel
                #:*lisp-exiting-p*)
  #+(:and :lispworks (:not :mswindows))
  (:import-from #:server/control-socket
                #:with-control-socket)
  (:import-from #:util/threading
                #:funcall-with-sentry-logs)
  (:import-from #:util/store/store-migrations
                #:run-migrations)
  #+lispworks
  (:import-from #:hunchentoot-extensions/existing-socket
                #:acceptor-with-existing-socket)
  (:export #:main
           #:register-acceptor
           #:slynk-loop
           #:*slynk-preparer*
           #:slynk-prepare
           #:*slynk-loopback-interface*
           #:*slynk-port*
           #:slynk-teardown
           #:*shutdown-hooks*
           #:with-cron))
(in-package #:server)

(defvar *port*)
(defvar *slynk-port*)
(defvar *verify-store*)
(defvar *profile-store*)
(defvar *verify-snapshots*)
(defvar *shell*)
(defvar *start-slynk*)

;; Reminder you can generate a self-signed pair like so:
;; openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem -sha256 -days 365 -nodes -subj '/CN=localhost'

(defvar *slynk-loopback-interface* "localhost")
(defvar *health-check*)

#-(:and :lispworks (:not :mswindows))
(def-easy-macro with-control-socket (&fn fn)
  (funcall fn))

(defparameter *options*
  `((*port* #+screenshotbot-oss "4091"
            #-screenshotbot-oss "4001" "" :params ("PORT"))
    (*shell* nil "")
    (*slynk-port* #+screenshotbot-oss "4095"
                  #-screenshotbot-oss "4005"
                  "" :params ("SLYNK-PORT"))
    (*slynk-loopback-interface* "localhost" "The interface on which we bind the slynk port"
                                :params ("SLYNK-LOOPBACK-INTERFACE"))
    (*start-slynk* #+screenshotbot-oss nil
                   #-screenshotbot-oss t
                   "")
    (util/store:*object-store*
     #+screenshotbot-oss "~/.config/screenshotbot/object-store/"
     #-screenshotbot-oss "/data/arnold/object-store/" "" :params ("OBJECT-STORE"))
    (*verify-store* nil "")
    (*health-check* nil "")
    #+lispworks
    (*profile-store* nil "When used with --verify-store, profiles the store load")
    (*verify-snapshots* nil "")
    #-sbcl
    (jvm:*libjvm* nil "Location of libjvm.so" :params ("LIBJVM"))
    (*debugger*
     nil
     "Enable the debugger on the main thread, this lets us debug issues with loading snapshots and transaction logs."
     :params ("DEBUGGER"))))

(defclass my-acceptor (#+lispworks acceptor-with-existing-socket
                       hunchentoot-multi-acceptor:multi-acceptor)
  ())

(defvar *multi-acceptor*)

(defvar *init-hooks* nil)

(defun register-acceptor (acceptor &rest hostnames)
  (loop for host in hostnames do
    (let ((host host))
     (push
      (lambda ()
        (log:info "Adding acceptor ~a" acceptor)
        (hunchentoot-multi-acceptor:add-acceptor *multi-acceptor* host acceptor))
      *init-hooks*)
      ;; Immediately register the acceptor if we're executing this live
      (when (boundp '*multi-acceptor*)
        (init-sub-acceptors)))))

(defvar *shutdown-hooks* nil)

(defun init-multi-acceptor (&key (port (parse-integer *port*)))
  (setf *multi-acceptor* (make-instance 'my-acceptor :port port :name 'multi-acceptor))
  (init-sub-acceptors))

(defun init-sub-acceptors ()
  (mapc 'funcall *init-hooks*)
  (setf *init-hooks* nil))

(defvar *slynk-preparer* nil)

(defmethod slynk-prepare (preparer)
  (values))

(defmethod slynk-teardown (preparer)
  (values))

(defvar *multi-server*)

(defclass utf-8-daily-file-appender (log4cl:daily-file-appender)
  ())

(defmethod slot-unbound (class (appender utf-8-daily-file-appender)
                         (slot-name (eql 'log4cl::stream)))
  (declare (ignore class slot-name))
  (create-appender-file appender))

(defun create-appender-file (appender)
  (let ((filename (log4cl::appender-filename appender)))
    (log4cl::maybe-close-stream appender)
    (setf (slot-value appender 'stream)
          (flexi-streams:make-flexi-stream
           (open (ensure-directories-exist filename)
                 #+ccl :sharing #+ccl :external
                 :direction :output
                 :if-exists :append
                 :element-type '(unsigned-byte 8)
                 :if-does-not-exist :create)
           :external-format :utf-8
           :element-type 'character))))

(defun setup-appenders (&key clear)
  (let ((log-file (path:catfile "log/logs")))
    (when clear
     (log4cl:clear-logging-configuration))
    (log:config :info)
    (log4cl:add-appender log4cl:*root-logger*
                         (make-instance 'utf-8-daily-file-appender
                                         :name-format log-file
                                         :backup-name-format
                                         "logs.%Y%m%d"
                                         :filter 4
                                         :layout (make-instance 'log4cl:simple-layout)))))

(defun setup-log4cl-debugger-hook ()
  )

(defvar *debugger* nil)

(def-easy-macro with-hidden-warnings (&fn fn)
  (let ((old *error-output*))
   (with-output-to-string (out)
     (setf *error-output* out)
     (unwind-protect
          (funcall fn)
       (setf *error-output* old)))))

(def-easy-macro with-lparallel-kernel (&key (threads 20)
                                            &fn fn)
  (let ((shutting-down-p nil))
    (flet ((context (fn)
             (handler-bind ((simple-warning (lambda (w)
                                              (when shutting-down-p
                                                (muffle-warning w)))))
               (funcall fn))))
      (setf lparallel:*kernel* (lparallel:make-kernel threads
                                                      :context #'context))
      (unwind-protect
           (funcall fn)
        (safe-log "lparallel: shutting down~%")
        (setf shutting-down-p t)
        (lparallel:end-kernel :wait t)
        (safe-log "lparallel: shutdown complete!~%")))))

(def-easy-macro maybe-with-debugger (&fn fn)
  (handler-bind ((error (lambda (e)
                          (safe-log "Got error: ~a (*debugger* is set to ~a)" e
                                    *debugger*)
                          #+lispworks
                          (dbg:output-backtrace :brief)
                          ;; So by this point, *debugger* is most
                          ;; likely already read from the command line
                          ;; options.
                          (unless *debugger*
                            ;; Invoking a restart here might be
                            ;; unsafe, since we don't know if all the
                            ;; unwind-protects are going to behave
                            ;; correctly.
                            (safe-log "Aborting because of error~%")
                            (uiop:quit 1)))))
    (fn))
  (safe-log "Out of maybe-with-debugger~%"))

(def-easy-macro with-sentry-logging (&fn fn)
  (funcall-with-sentry-logs fn))

(def-easy-macro with-slynk (&fn fn)
  (when *start-slynk*
    (slynk-prepare *slynk-preparer*))
  (unwind-protect
       (fn)
    (safe-log "Shutting down slynk~%")
    (slynk-teardown *slynk-preparer*)))

(def-easy-macro with-running-acceptor (acceptor &fn fn)
  (hunchentoot:start acceptor)
  (unwind-protect
       (fn)
    (safe-log "Shutting down hunchentoot~%")
    (hunchentoot:stop acceptor)))

(def-easy-macro with-cron (&fn fn)
  (cl-cron:start-cron)
  (unwind-protect
       (funcall fn)
    (safe-log "Shutting down cron~%")
    (cl-cron:stop-cron)))

(def-easy-macro with-cl-cli-processed (&key &binding verify-store
                                            &binding profile-store
                                            &binding health-check
                                            &fn fn)
  (let ((args #-lispworks (cons "<arg0>"(uiop:command-line-arguments))
                    #+lispworks sys:*line-arguments-list*))
          (safe-log "CLI args: ~s~%" args)
    (multiple-value-bind (vars vals matched dispatch rest)
              (cl-cli:parse-cli args
                                *options*)
            (declare (ignore matched dispatch rest))
            (loop for var in vars
                  for val in vals
                  do (setf (symbol-value var) val))
      (fn *verify-store*
        (or #+lispworks *profile-store*)
        *health-check*))))

(defun safe-log (&rest args)
  (apply #'format t args)
  (finish-output t))

(def-easy-macro with-common-setup (&key enable-store jvm &fn fn)
  (maybe-with-debugger ()
    (with-sentry-logger ()
     (with-lparallel-kernel ()
       (with-control-socket ()
         (with-slynk ()
           (unwind-on-interrupt ()
             #+lispworks
             (when jvm
               (jvm:jvm-init))

             (fn))))
       ;; unwind if an interrupt happens
       (log:config :sane :immediate-flush t)
       (log:config :info)
       (safe-log "shutting down~%")
       (safe-log "calling shutdown hooks~%")
       (mapc 'funcall *shutdown-hooks*)

       ;; ;; don't snapshot the store, if the process is killed while the
       ;; ;; snapshot is happening, we have to manually recover the store
       ;; (bknr.datastore:snapshot)

       (when enable-store
         (bknr.datastore:close-store))

       (safe-log "all services down~%")
       (finish-out))))

  #+lispworks
  (wait-for-processes)
  (safe-log "all threads before exiting: ~s~%" (bt:all-threads))
  (log4cl:flush-all-appenders)
  (log4cl:stop-hierarchy-watcher-thread))

(defun main (&key (enable-store t)
               (jvm t)
               acceptor)
  "called from launch scripts, either web-bin or launch.lisp"

  (with-cl-cli-processed (:verify-store verify-store
                          :profile-store profile-store
                          :health-check health-check)
    (declare (ignorable profile-store))
    (cond
      (verify-store
       (with-common-setup (:enable-store t :jvm jvm)
         (%verify :profile-store profile-store)))
      (health-check
       (with-common-setup (:enable-store enable-store :jvm jvm)
         (run-health-checks)
         (uiop:quit 0)))
      (t
       (with-common-setup (:enable-store enable-store :jvm jvm)
         (%run :enable-store enable-store :acceptor acceptor))))))

(defun %verify (&key profile-store)
  (log:config :info)
  (time
   (cond
     #+lispworks
     (profile-store
      (hcl:profile
       (util/store:verify-store)))
     (t
      (util/store:verify-store
       :callback (lambda ()
                   (log:info "Done verifying store, running health checks...")
                   (unless (run-health-checks)
                     (log:info "Crashing, since health checks failed")
                     (uiop:quit 1))
                   (run-migrations :snapshot nil))))))

  (uiop:quit 0))

(defun required (&optional arg)
  (error "Missing required argument: ~a" arg))

(defun %run (&key (enable-store (required))
               (acceptor (required))
               (port (parse-integer *port*))
               (shell *shell*))
  (log:info "The port is now ~a" port)

  (setup-appenders)

  (setup-log4cl-debugger-hook)


  ;; set this to t for 404 page. :/
  (setf hunchentoot:*show-lisp-errors-p* t)

  (setf hunchentoot:*rewrite-for-session-urls* nil)

  (when enable-store
    (util/store:prepare-store))

  #+screenshotbot-oss
  (run-migrations)

  (log:info "Store is prepared, moving on...")
  (with-cron ()
    (cond
      (shell
       (log:info "Slynk has started up, but we're not going to start hunchentoot. Call (QUIT) from slynk when done."))
      (t
       (unless acceptor
         (init-multi-acceptor :port port)
         (setf acceptor *multi-acceptor*))
       (with-running-acceptor (acceptor)
         (log:info "The web server is live at port ~a. See logs/logs for more logs~%"
                   (hunchentoot:acceptor-port acceptor))
         (setup-appenders :clear t)

         (log:info "Now we wait indefinitely for shutdown notifications")
         (loop (sleep 60)))))))

#+lispworks
(defun wait-for-processes ()
  (dotimes (i 30)
    (safe-log "[~a/30] Wait for processes" i)
    (let* ((processes
             (set-difference (mp:list-all-processes) mp:*initial-processes*))
           (processes
             (loop for p in processes
                   unless (or
                           (eql p (mp:get-current-process))
                           (member (mp:process-name p)
                                   '("Hierarchy Watcher"
                                     "The idle process"
                                     "Initial delivery process"
                                     "Restart Function Process")
                                   :test 'string=))
                     collect p)))

      (cond
        (processes
         (log:info "Threads remaining: ~S" processes)
         (log4cl:flush-all-appenders)
         (sleep 1))
        (t
         ;; nothing left!
         (log:info "Should be a safe shutdown!")
         (return-from wait-for-processes nil)))))
  (log:info "We waited for threads to cleanup but nothing happened, so we're going for a force uiop:quit")
  (log4cl:flush-all-appenders)
  (uiop:quit))
