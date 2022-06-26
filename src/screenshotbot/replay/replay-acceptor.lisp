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
              :initform (make-hash-table :test #'equal)))
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
        snapshot))

(defmethod pop-snapshot ((acceptor render-acceptor)
                         (snapshot replay:snapshot))
  (remhash (format nil "~a" (replay:uuid snapshot))  (acceptor-snapshots acceptor)))


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
                              :defaults f)))))
     (let ((input-file (fix-input-file (replay:snapshot-asset-file snapshot asset))))
       (setf (hunchentoot:return-code*)
             (replay:asset-status asset))
       (loop for header in (replay:asset-response-headers asset)
             for key = (replay:http-header-name header)
             for val = (replay:http-header-value header)
             do
                (unless (member key (list "transfer-encoding") :test #'string-equal)
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
                          val)))))
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
         (uuid (elt (str:split "/" script-name) 2))
         (snapshot (gethash uuid (acceptor-snapshots hunchentoot:*acceptor*))))
    (unless snapshot
      (error "Could not find snapshot for uuid `~a`" uuid))
    (call-with-request-counter
     snapshot
     (lambda ()
       (loop for asset in (replay:assets snapshot)
             if (string= script-name (replay:asset-file asset))
               do
                  (return
                    (handle-asset snapshot asset))
             finally
                (progn
                  (log:error "No such asset: ~a" script-name)
                  (setf (hunchentoot:return-code*) 404)))))))

(define-easy-handler (wait-for-zero :uri "/wait-for-zero-requests"
                                    :acceptor-names '(replay))
    (uuid (timeout :parameter-type 'integer :init-form 10))
  (log:info "Wait for zero requests at ~a" (get-universal-time))
  (unwind-protect
       (let* ((snapshot (gethash uuid (acceptor-snapshots hunchentoot:*acceptor*)))
              (start-time (local-time:now))
              (last-non-zero (local-time:now)))

         (loop
           (progn
             (cond
               ((> (request-counter snapshot) 0)
                (log:info "Request counter: ~a" (request-counter snapshot))
                (setf last-non-zero (local-time:now)))
               ((let ((now (now)))
                  (or (timestamp>= now
                                   (timestamp+ last-non-zero timeout :sec))
                      (timestamp>= now
                                   (timestamp+ start-time (* 10 timeout) :sec))))
                (return)))
             (sleep 0.1)))))
  (log:info "End waiting for zero requests at ~a" (get-universal-time)))


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
