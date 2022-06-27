;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage #:screenshotbot/replay/replay-acceptor
  (:use #:cl)
  (:nicknames #:screenshotbot/pro/replay/replay-acceptor)
  (:import-from #:hunchentoot
                #:*acceptor*
                #:define-easy-handler)
  (:import-from #:screenshotbot/replay/core
                #:parse-max-age
                #:asset-file-name
                #:asset-file
                #:assets
                #:snapshot
                #:request-counter
                #:call-with-request-counter)
  (:import-from #:local-time
                #:timestamp+
                #:now
                #:timestamp>=)
  (:import-from #:auto-restart
                #:with-auto-restart)
  (:import-from #:screenshotbot/server
                #:register-init-hook
                #:*init-hooks*)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/object-id
                #:find-by-oid)
  (:export
   #:call-with-hosted-snapshot
   #:render-acceptor
   #:push-snapshot)
  (:local-nicknames (#:a #:alexandria)
                    (#:replay #:screenshotbot/replay/core)))
(in-package #:screenshotbot/replay/replay-acceptor)

(defun document-root ()
  (asdf:system-relative-pathname :screenshotbot.pro "replay/static/"))



(defclass render-acceptor (hunchentoot:easy-acceptor)
  ((snapshots :reader acceptor-snapshots
              :initform (make-hash-table :test #'equal))
   (asset-maps :reader asset-maps
               :initform (make-hash-table)
               :documentation "For each snapshot, a map from filename to asset")
   (snapshots-company
    :initform nil
    :accessor snapshots-company
    :documentation "A list of company and snapshot pairs"))
  (:default-initargs :name 'replay
                     :port 5002
                     :access-log-destination nil
                     :message-log-destination nil))

(defvar *default-render-acceptor* nil)

(defun default-render-acceptor ()
  (util:or-setf
   *default-render-acceptor*
   (let ((acceptor (make-instance 'render-acceptor)))
     (hunchentoot:start acceptor)
     acceptor)
   :thread-safe t))

(defmethod initialize-instance :after ((acceptor render-acceptor) &key snapshot
                                       &allow-other-keys)
  (when snapshot
    (error "OBSOLETE: passing snapshot as initarg")))

(defmethod push-snapshot ((acceptor render-acceptor)
                          (company company)
                          (snapshot replay:snapshot))
  (setf (gethash (format nil "~a" (replay:uuid snapshot)) (acceptor-snapshots acceptor))
        snapshot)
  (setf (snapshots-company acceptor)
        (acons
         snapshot company
         (snapshots-company acceptor)))
  (let ((asset-map (make-hash-table :test #'equal)))
    (dolist (asset (assets snapshot))
      (setf (gethash (asset-file-name asset) asset-map) asset))
    (setf (gethash snapshot (asset-maps acceptor))
          asset-map)))

(defmethod pop-snapshot ((acceptor render-acceptor)
                         (snapshot replay:snapshot))
  (a:deletef (snapshots-company acceptor)
             snapshot :key #'car)
  (remhash (format nil "~a" (replay:uuid snapshot))  (acceptor-snapshots acceptor))
  (remhash snapshot (Asset-maps acceptor)))


(define-easy-handler (root :uri "/root" :acceptor-names '(replay)) ()
  (let ((snapshot (car (loop for snapshot being the hash-values of (acceptor-snapshots *acceptor*)
                             collect snapshot))))
   (handle-asset
    snapshot
    (car (replay:root-assets snapshot)))))

(define-easy-handler (debug-replay :uri "/debug" :acceptor-names '(replay)) ()
  (format nil "snapshots: ~S"
          (loop for key being the hash-keys of  (acceptor-snapshots hunchentoot:*acceptor*)
                collect key)))

(define-easy-handler (iframe-not-support
                      :uri "/iframe-not-supported"
                      :acceptor-names '(replay)) ()
  "<h1>iframe removed by Screenshotbot</h1>")

(define-easy-handler (replay.css :uri "/css/replay.css" :acceptor-names '(replay)) ()
  (let ((file (path:catfile (document-root) "css/replay.css")))
   (hunchentoot:handle-static-file
    file)))

(defun set-cache-control (seconds)
  (setf (hunchentoot:header-out "cache-control" hunchentoot:*reply*)
        (format nil "max-age=~d" seconds)))

(with-auto-restart ()
 (defun handle-asset (snapshot asset)
   (log:info "Starting with ~a" asset)
   (flet ((fix-input-file (f)
            (cond
              ((uiop:file-exists-p f)
               f)
              (t
               ;; hack: please remove
               (make-pathname :type "tmp"
                              :defaults f))))
          (set-minimum-cache ()
            (set-cache-control 300)))
     (set-minimum-cache)
     (let ((input-file (fix-input-file (replay:snapshot-asset-file snapshot asset))))
       (setf (hunchentoot:return-code*)
             (replay:asset-status asset))
       (loop for header in (replay:asset-response-headers asset)
             for key = (replay:http-header-name header)
             for val = (replay:http-header-value header)
             do
                (cond
                  ((member key (list "transfer-encoding") :test #'string-equal)
                   ;; do nothing
                   nil)
                  ((and
                    (string-equal "cache-control" key)
                    (< (parse-max-age val) 300))
                   (set-minimum-cache))
                  (t
                   (setf (hunchentoot:header-out key hunchentoot:*reply*)
                         (cond
                           ((string-equal "content-length" key)
                            ;; hunchentoot has special handling for
                            ;; content-length. But also, we might have
                            ;; modified the file since we downloaded it, so
                            ;; we should use the updated length.
                            (assert (uiop:file-exists-p input-file))
                            (with-open-file (input input-file)
                              (file-length input)))
                           (t
                            val))))))
       (handler-case
           (let ((out (hunchentoot:send-headers)))
             (assert (uiop:file-exists-p input-file))
             (when (uiop:file-exists-p input-file)
               (with-open-file (input input-file
                                      :element-type '(unsigned-byte 8))
                 (uiop:copy-stream-to-stream input out :element-type '(unsigned-byte 8))))
             (finish-output out))
         #+lispworks
         (comm:socket-io-error ()))
       (log:info "Done with ~a" asset)))))

(defvar *lock* (bt:make-lock))
(define-easy-handler (asset :uri (lambda (request)
                                   (let ((script-name (hunchentoot:script-name request)))
                                    (and
                                     (str:starts-with-p "/snapshot/" script-name)
                                     (str:containsp "/assets/" script-name))))
                            :acceptor-names '(replay))
    ()
  (let* ((script-name (hunchentoot:script-name hunchentoot:*request*))
         (parts (str:split "/" script-name))
         (uuid (elt parts 2))
         (asset-file-name (elt parts 4))
         (snapshot (gethash uuid (acceptor-snapshots hunchentoot:*acceptor*))))
    (unless snapshot
      (error "Could not find snapshot for uuid `~a`" uuid))
    (call-with-request-counter
     snapshot
     (lambda ()
       (let* ((asset-map (gethash snapshot (asset-maps hunchentoot:*acceptor*)))
              (asset (gethash asset-file-name asset-map)))
         (cond
           (asset
            (handle-asset snapshot asset))
           (t
            (send-404 script-name))))))))

(defun send-404 (script-name)
  (log:error "No such asset: ~a" script-name)
  (setf (hunchentoot:return-code*) 404)
  (set-cache-control 3600)
  "No such asset")

(define-easy-handler (asset-from-company
                      :uri (lambda (request)
                             (let ((script-name (hunchentoot:script-name request)))
                               (and
                                (str:starts-with-p "/company/" script-name)
                                (str:containsp "/assets/" script-name))))
                            :acceptor-names '(replay))
    ()
  (let* ((script-name (hunchentoot:script-name hunchentoot:*request*))
         (parts (str:split "/" script-name))
         (company-oid (encrypt:decrypt-mongoid (elt parts 2)))
         (asset-file-name (elt parts 4))
         (company (find-by-oid company-oid)))
    (check-type company company)
    (handle-asset-from-company
     hunchentoot:*acceptor*
     company
     asset-file-name)))

(defun handle-asset-from-company (acceptor company asset-file-name)
  (loop for (snapshot . check-company) in (snapshots-company acceptor)
        for asset-map = (gethash snapshot (asset-maps acceptor))
        for asset = (gethash asset-file-name asset-map)
        if (and (eql company check-company) asset)
          do
             (return
               (handle-asset snapshot asset))
        finally
        (send-404 asset-file-name)))


(defun hostname ()
  (cond
    ((and
      (uiop:file-exists-p "/.dockerenv")
      (equal "thecharmer" (uiop:hostname)))
     "staging")
    (t
     "replay")))

(defmethod call-with-hosted-snapshot ((company company)
                                      (snapshot snapshot)
                                      fn &key (hostname (hostname)))
  (assert (functionp fn))
  (push-snapshot (default-render-acceptor) company snapshot)
  (unwind-protect
       (let ((acceptor (default-render-acceptor))
             (root-asset (car (replay:root-assets snapshot))))
         (progn
           (funcall fn (format nil "http://~a:~a~a"
                               hostname  (hunchentoot:acceptor-port acceptor)
                               (replay:asset-file root-asset)))))
    (pop-snapshot (default-render-acceptor) snapshot)))
