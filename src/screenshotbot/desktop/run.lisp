;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/desktop/run
  (:use #:cl)
  (:import-from #:server
                #:with-cron
                #:with-lparallel-kernel)
  (:import-from #:util/misc
                #:with-global-binding)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:screenshotbot/installation
                #:installation
                #:desktop-installation)
  (:import-from #:screenshotbot/model/user
                #:user)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/assets
                #:*asset-list*)
  (:import-from #:screenshotbot/api-key-api
                #:api-key-secret-key
                #:api-key-key
                #:api-key)
  (:import-from #:screenshotbot/model/company
                #:get-singleton-company)
  (:import-from #:screenshotbot/api/core
                #:authenticate-api-request)
  (:export
   #:command))
(in-package :screenshotbot/desktop/run)


(defun make-pre-compiled-assets ()
  (let ((res (make-hash-table)))
    (loop for asset in *asset-list*
          do
             (asdf:compile-system asset)
             (setf
              (gethash asset res)
              (mapcar #'uiop:read-file-string
                      (asdf:output-files 'asdf:compile-op asset))))
    res))

(defvar *pre-compiled-assets* (make-pre-compiled-assets))

(defclass desktop-acceptor (screenshotbot/server:acceptor)
  ()
  (:default-initargs
   :request-class 'desktop-request
   :address "127.0.0.1"))

(defclass desktop-request (screenshotbot/server:request)
  ())

(defun singleton-user ()
  (or
   (car (bknr.datastore:class-instances 'user))
   (let ((username (or (uiop:getenv "USER") "user")))
     (make-instance 'user
                    :full-name username
                    :companies (list (get-singleton-company (installation)))
                    :email (format nil "~a@localhost" username)))))

(defmethod auth:request-user ((request desktop-request))
  (singleton-user))

(defmethod authenticate-api-request ((request desktop-request))
  ;; Always authenticated in desktop mode!
  (values))

(defun store-dir ()
  (ensure-directories-exist
   (path:catdir (format nil "~a/" (uiop:getenv "HOME"))
                ".config/screenshotbot/desktop-store/")))

(def-easy-macro with-store (&fn fn)
  (with-global-binding ((util/store:*object-store*
                         (namestring
                          (store-dir))))
    (util/store:prepare-store)
    (unwind-protect
         (fn)
      (bknr.datastore:close-store))))

(defun ensure-desktop-api-key ()
  (or
   (car (bknr.datastore:class-instances 'api-key))
   (let ((key (make-instance 'api-key
                             :user (singleton-user)
                             :company (get-singleton-company (installation)))))
     (with-open-file (output (path:catfile (store-dir)
                                           "api-key")
                             :direction :output
                             :if-exists :supersede)
       (format output "~a:~a" (api-key-key key)
               (api-key-secret-key key))))))

(defun handler (cmd)
  (with-lparallel-kernel (:threads 4)
    (with-cron ()
      (with-global-binding ((*installation*
                             (make-instance 'desktop-installation
                                            :pre-compiled-assets *pre-compiled-assets*)))
        (with-store ()
          (ensure-desktop-api-key)
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
