;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/localhost/run
  (:use #:cl)
  (:import-from #:server
                #:with-cron
                #:with-lparallel-kernel)
  (:import-from #:util/misc
                #:with-global-binding)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:screenshotbot/installation
                #:singleton-company
                #:desktop-installation)
  (:import-from #:screenshotbot/model/user
                #:user)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:export
   #:command))
(in-package :screenshotbot/localhost/run)

(defclass desktop-acceptor (screenshotbot/server:acceptor)
  ()
  (:default-initargs
   :request-class 'desktop-request))

(defclass desktop-request (screenshotbot/server:request)
  ())

(defmethod auth:request-user ((request desktop-request))
  (or
   (car (bknr.datastore:class-instances 'user))
   (let ((username (or (uiop:getenv "USER") "user")))
     (make-instance 'user
                    :full-name username
                    :companies (list (singleton-company))
                    :email (format nil "~a@localhost" username)))))

(def-easy-macro with-store (&fn fn)
  (with-global-binding ((util/store:*object-store*
                         (namestring
                          (ensure-directories-exist
                           (path:catdir (format nil "~a/" (uiop:getenv "HOME"))
                                        ".config/screenshotbot/desktop-store/")))))
    (util/store:prepare-store)
    (unwind-protect
         (fn)
      (bknr.datastore:close-store))))

(defun handler (cmd)
  (with-lparallel-kernel (:threads 4)
    (with-cron ()
      (with-global-binding ((*installation*
                             (make-instance 'desktop-installation)))
        (with-store ()
         (let ((port (clingon:getopt cmd :port)))
           (let ((acceptor (make-instance 'desktop-acceptor
                                          :port port)))
             (screenshotbot/server::prepare-acceptor-plugins acceptor)
             (hunchentoot:start acceptor)
             (log:info "The server is ready on http://localhost:~a" port)
             (loop (sleep 60)))))))))

(defun options ()
  (list
   (clingon:make-option
    :integer
    :description "The port to host on, defaults to 4095"
    :short-name #\p
    :long-name "port"
    :initial-value "4095"
    :key :port)))

(defun command ()
  (clingon:make-command :name "run"
                        :description "Run the screenshotbot service"
                        :options (options)
                        :handler #'handler))