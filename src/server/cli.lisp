(defpackage :server/cli
  (:use #:cl)
  (:import-from #:clingon
                #:make-option)
  (:import-from #:server
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
  (:export
   #:main))
(in-package :server/cli)

(defun run/command ()
  (clingon:make-command :name "run"
                        :options (list)))

(def-easy-macro with-store (cmd &fn fn)
  (with-global-binding ((*object-store* (serapeum:ensure-suffix (clingon:getopt cmd :store) "/"))
                        (*start-slynk* (clingon:getopt cmd :start-slynk))
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

(defun verify/command (&key enable-store jvm)
  (clingon:make-command
   :name "verify"
   :handler (lambda (cmd)
              (with-store (cmd)
                (with-common-setup (:enable-store enable-store :jvm jvm)
                  (%verify :profile-store (clingon:getopt cmd :profile)))))
   :options (list*
             (make-option
              :flag
              :description "Whether to run the profiler when verifying the store"
              :initial-value nil
              :long-name "profile"
              :key :profile)
             (common-options))))

(defun run/command (&key enable-store jvm acceptor)
  (clingon:make-command
   :name "run"
   :handler (lambda (cmd)
              (with-store (cmd)
                (with-common-setup (:enable-store enable-store :jvm jvm)
                  (%run :enable-store enable-store
                        :acceptor acceptor
                        :port (clingon:getopt cmd :port)
                        :shell nil))))
   :options (list*
             (make-option
              :integer
              :description "HTTP access port"
              :long-name "port"
              :initial-value 4001
              :key :port)
             (common-options))))

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
    :flag
    :description "Whether to start slynk"
    :long-name "start-slynk"
    :initial-value :true
    :key :start-slynk)
   (make-option
    :integer
    :description "the port to start slynk on"
    :long-name "slynk-port"
    :initial-value 4005
    :key :slynk-port)))

(defun main/command (&key enable-store jvm acceptor)
  (clingon:make-command :name "App Server"
                        :handler #'main/handler
                        :sub-commands (list
                                       (self-test/command :enable-store enable-store
                                                          :jvm jvm)
                                       (verify/command :enable-store enable-store
                                                       :jvm jvm)
                                       (run/command :enable-store enable-store
                                                    :jvm jvm
                                                    :acceptor acceptor))))

(defun legacy-mode-p (args)
  (and (second args)
       (eql #\- (elt (second args) 0))))

(defun main (&key (jvm t) acceptor (enable-store t))
  (cond
    ((legacy-mode-p sys:*line-arguments-list*)
     (warn "Using legacy mode for command line parsing")
     (server:main :jvm jvm :acceptor acceptor :enable-store enable-store))
    (t
     (let ((args #-lispworks (cdr (uiop:raw-command-line-arguments))
                 #+lispworks (cdr sys:*line-arguments-list*)))
       (let ((app (main/command :jvm jvm :enable-store enable-store
                                :acceptor acceptor)))
         (clingon:run app args))))))
