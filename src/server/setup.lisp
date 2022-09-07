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
  (:export #:main
           #:register-acceptor
           #:slynk-loop
           #:*slynk-preparer*
           #:slynk-prepare
           #:*slynk-loopback-interface*
           #:*slynk-port*
           #:slynk-teardown))
(in-package #:server)

(defvar *port*)
(defvar *slynk-port*)
(defvar *verify-store*)
(defvar *verify-snapshots*)
(defvar *socketmaster*)
(defvar *shell*)
(defvar *start-slynk*)

;; Reminder you can generate a self-signed pair like so:
;; openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem -sha256 -days 365 -nodes -subj '/CN=localhost'

(defvar *ssl-key*)
(defvar *ssl-cert*)

(defvar *remote-debugging-client-port*)
(defvar *remote-debugging-password*)
(defvar *slynk-loopback-interface*)

(defparameter *options*
  `((*port* #+screenshotbot-oss "4091"
            #-screenshotbot-oss "4001" "" :params ("PORT"))
    (*socketmaster* nil "")
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
    (*verify-snapshots* nil "")
    #-sbcl
    (jvm:*libjvm* nil "Location of libjvm.so" :params ("LIBJVM"))
    (*ssl-key* nil "SSL Key to be used for remote-debugging-client and other services"
               :params ("SSL-KEY"))
    (*ssl-cert* nil "Corresponding SSL Certificate file"
                :params ("SSL-CERT"))
    (*remote-debugging-client-port*
     nil
     "A port to open a Lispworks remote debugging client. If SSL key is
 provided, we'll attach an SSL context to the socket"
     :params ("REMOTE-DEBUGGING-CLIENT-PORT"))
    (*remote-debugging-password*
     nil
     "A password required for a client to connect"
     :params ("REMOTE-DEBUGGING-CLIENT"))))

(defclass my-acceptor (hunchentoot-multi-acceptor:multi-acceptor)
  ())

;; When using socketmaster to restart the server without downtime, we
;; don't open a brand new socket, instead using the socket on file
;; descriptor 3. Currently this is only tested on Lispworks, and we
;; only use this for the non-OSS port. If you want to try this out,
;; try remove this feature check and see if it works.
#+(and lispworks (not screenshotbot-oss))
(defmethod hunchentoot:start-listening ((acceptor my-acceptor))
  ;; always listen on the PORT setup on fd 3. Assuming we'll be
  ;; started by socketmaster
  (cond
    (*socketmaster*
     (error "Socketmaster no longer supported, needs patched hunchentoot")
     #+nil
     (setf (hunchentoot::acceptor-listen-socket acceptor)
           (usocket::make-stream-server-socket 3 :element-type '(unsigned-byte 8))))
    (t
     (call-next-method))))

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

(defun init-multi-acceptor ()
  (setf *multi-acceptor* (make-instance 'my-acceptor :port (parse-integer *port*) :name 'multi-acceptor))
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

(defvar *remote-debugging-process* nil)

#+lispworks
(defun start-remote-debugging-client (port)
  (log:info "Starting remote debugging server on port ~a" port)
  (let ((ssl-ctx (when *ssl-key*
                   (log:info "Using SSL key at ~a, cert at ~a" *ssl-key* *ssl-cert*)
                   (comm:create-ssl-server-context :key-file *ssl-key*
                                                   :cert-file *ssl-cert*
                                                   :implementation :openssl))))
    (setf *remote-debugging-process*
     (util/remote-debugging:start-client-remote-debugging-server
      :port port
      :password *remote-debugging-password*
      :ssl ssl-ctx)))
  (log:info "Remote debugging server started"))

(defun main (&key (enable-store t)
               (jvm t)
               acceptor)
  "Called from launch scripts, either web-bin or launch.lisp"

  (unwind-on-interrupt ()
      (progn
        (let ((args #-lispworks (cons "<arg0>"(uiop:command-line-arguments))
                    #+lispworks sys:*line-arguments-list*))
          (log:info "CLI args: ~s" args)


          (multiple-value-bind (vars vals matched dispatch rest)
              (cl-cli:parse-cli args
                                *options*)
            (declare (ignore matched dispatch rest))
            (loop for var in vars
                  for val in vals
                  do (setf (symbol-value var) val))

            (when *start-slynk*
              (slynk-prepare *slynk-preparer*))

            #+lispworks
            (when *remote-debugging-client-port*
              (start-remote-debugging-client
               *remote-debugging-client-port*))

            (when *verify-store*
              (log:config :info)
              (time
               (util/store:verify-store))
              (log:info "Done verifying store")
              (log:info "Running health checks...")
              (run-health-checks)
              (uiop:quit 0))

            #+nil
            (when *verify-snapshots*
              (log:config :info)
              (util:verify-snapshots)
              (uiop:quit 0))

            (log:info "The port is now ~a" *port*)

            (unless acceptor
              (init-multi-acceptor)
              (setf acceptor *multi-acceptor*))
            #+lispworks
            (when jvm
             (jvm:jvm-init))
            (setup-appenders)

            (setup-log4cl-debugger-hook)


            #+sbcl
            (progn
              (format t "Using file descriptor ~A~%" listen-fd)
              (setf (hunchentoot-multi-acceptor:listen-fd *multi-acceptor*) listen-fd))

            ;; set this to t for 404 page. :/
            (setf hunchentoot:*show-lisp-errors-p* t)

            (setf hunchentoot:*rewrite-for-session-urls* nil)

            (when enable-store
             (util/store:prepare-store))

            (cl-cron:start-cron)

            (cond
              (*shell*
               (log:info "Slynk has started up, but we're not going to start hunchentoot. Call (QUIT) from slynk when done."))
              (t
               (hunchentoot:start acceptor)))

            (log:info "The web server is live at port ~a. See logs/logs for more logs~%"
                      (hunchentoot:acceptor-port acceptor))

            (setup-appenders :clear t)

            (log:info "Now we wait indefinitely for shutdown notifications"))))

    ;; unwind if an interrupt happens
    (log:config :sane :immediate-flush t)
    (log:config :info)
    (log:info "SHUTTING DOWN~%")
    (finish-output t)
    (log:info "Shutting down cron")
    (cl-cron:stop-cron)
    (log:info "Shutting down hunchentoot")
    (hunchentoot:stop acceptor)

    ;;;; Don't snapshot the store, if the process is killed while the
    ;;;; snapshot is happening, we have to manually recover the store
    ;; (bknr.datastore:snapshot)

    (when enable-store
      (bknr.datastore:close-store))

    #+lispworks
    (when *remote-debugging-process*
      (comm:server-terminate *remote-debugging-process*))

    (log:info "Shutting down slynk")
    (slynk-teardown *slynk-preparer*)
    (log:info "All services down")
    #+lispworks
    (wait-for-processes)
    (log:info "All threads before exiting: ~s" (bt:all-threads))
    (log4cl:flush-all-appenders)
    (log4cl:stop-hierarchy-watcher-thread)))

#+lispworks
(defun wait-for-processes ()
  (dotimes (i 30)
   (let* ((processes
           (set-difference (mp:list-all-processes) mp:*initial-processes*))
          (processes
           (loop for p in processes
              unless (member (mp:process-name p)
                             '("Hierarchy Watcher"
                               "The idle process"
                               "Initial delivery process"
                               "Restart Function Process")
                             :test 'string=)
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
