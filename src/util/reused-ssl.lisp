;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/reused-ssl
  (:use #:cl)
  (:import-from #:util/request
                #:http-request
                #:low-level-request
                #:engine)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:util/truncated-stream
                #:delegate
                #:truncated-stream)
  (:export
   #:with-reused-ssl)
  (:local-nicknames #-lispworks
                    (#:stream #:trivial-gray-streams)))
(in-package :util/reused-ssl)

(defvar *timeout* 10)

(defclass connection ()
  ((last-use-time :initarg :last-use-time
                  :accessor last-use-time
                  :initform (get-universal-time))
   (stream :initarg :stream
           :reader connection-stream)
   (domain :initarg :domain
           :reader connection-domain))
  (:documentation "A connection that has been kept-alive"))

(defclass reuse-context ()
  ((connections :initarg :connections
                :accessor connections
                :initform (make-hash-table :test #'equal))))

(defmethod find-connection ((self reuse-context) domain)
  (trim-old-connections self)
  (symbol-macrolet ((stack (gethash domain (connections self))))
    (log:debug "hash-table is ~S" (connections self))
    (prog1
        (pop stack)
      (log:debug "Length of connections is ~a" (length stack)))))

(defmethod trim-old-connections ((self reuse-context))
  (let ((now (get-universal-time)))
   (loop for key being the hash-keys of (connections self)
           using (hash-value connections)
         do
            (setf
             (gethash key (connections self))
             (delete-if (lambda (connection)
                          (< (last-use-time connection) (- now *timeout*)))
                        connections)))))

(defmethod find-connection ((self null) domain)
  nil)

(defmethod %cleanup ((self reuse-context))
  (loop for domain being the hash-keys of (connections self)
        using (hash-value connections) do
    (loop for connection in connections
          do
             (log:debug "Cleaning up: ~a" (connection-stream connection))
             (close (connection-stream connection))))
  (setf (connections self) nil))

(defvar *reuse-contexts* nil)

(defclass reused-ssl-mixin ()
  ())

(defmethod reuse-context (engine)
  (assoc-value *reuse-contexts* engine))

(defclass reused-ssl-engine (reused-ssl-mixin
                             engine)
  ())

(defvar *default-reused-ssl-engine*
  (make-instance 'reused-ssl-engine))


(defclass tracked-stream (truncated-stream)
  ((reusable-stream :initarg :reusable-stream
                    :initform (error "must provide :reusable-stream")
                    :reader reuseable-stream
                    :documentation "The underlying stream that can be reused when closed, this might be different from the delegate!")
   (closedp :initform nil
            :accessor closedp)
   (domain :initarg :domain
           :reader domain)
   (reuse-context :initarg :reuse-context
                  :reader reuse-context))
  (:documentation "A stream that once closed, we'll reuse the underlying stream"))

(def-easy-macro with-reused-ssl (engine &key &binding reuse-context &fn fn)
  (let ((reuse-context (make-instance 'reuse-context)))
    (unwind-protect
         (let ((*reuse-contexts*
                 (acons
                  engine reuse-context
                  *reuse-contexts*)))
          (fn reuse-context))
      (%cleanup reuse-context))))

(defmethod stream::stream-element-type ((self tracked-stream))
  ;; Delete
  (call-next-method))

(defmethod stream:stream-read-sequence ((self tracked-stream)
                                        sequence
                                        start
                                        end
                                        #-lispworks #-lispworks
                                                    &key &allow-other-keys)
  ;; Delete
  (call-next-method))

#+nil
(defmethod stream:stream-check-eof-no-hang ((self tracked-stream))
  (stream:stream-check-eof-no-hang (delegate self)))

#+nil
(defmethod stream:stream-read-char ((self tracked-stream))
  (stream:stream-read-char (delegate self)))

#+nil
(defmethod stream:stream-read-char-no-hang ((self tracked-stream))
  (stream:stream-read-char-no-hang self))

(defmethod stream:stream-read-sequence ((self tracked-stream)
                                        sequence
                                        start
                                        end
                                        #-lispworks #-lispworks
                                                    &key &allow-other-keys)
  (cond
    ((closedp self)
     (error "The stream is already closed"))
    (t
     (call-next-method))))

(defun push-connection (reuse-context domain stream)
  (push
   (make-instance 'connection
                  :domain domain
                  :stream stream)
   (gethash domain (connections reuse-context))))

(defmethod close ((self tracked-stream) &key abort)
  (declare (ignore abort))
  ;; Rather than really closing, push connection
  (cond
    ((not (closedp self))
     (log:debug "Closing a tracked-stream")
     (setf (closedp self) t)
     (push-connection
      (reuse-context self)
      (domain self)
      (delegate self)))
    (t
     (log:debug "The tracked-stream is already closed"))))

(defmethod stream:stream-finish-output ((self tracked-stream))
  ;; Delete
  (call-next-method))

(defmethod stream:stream-force-output ((self tracked-stream))
  ;; Delete
  (call-next-method))

(defmethod low-level-request ((self reused-ssl-mixin)
                              url &rest args
                              &key want-stream
                              &allow-other-keys)
  (let ((reuse-context (assoc-value *reuse-contexts* self))
        (loggable-args (ignore-errors
                        (alexandria:remove-from-plist args
                                                      :basic-authorization))))
    (cond
     ((not reuse-context)
      (warn "no reuse-context available")
      (call-next-method))
     (t
      (multiple-value-bind (stream code headers
                            uri
                            underlying-stream
                            should-close)
          (anaphora:acond
            ((find-connection (assoc-value *reuse-contexts* self)
                              (tracked-stream-key
                               (puri:uri url)))
             (let* ((connection anaphora:it))
               (log:debug "Reusing an existing stream ~a, ~a" (connection-stream connection)
                          loggable-args)
               (apply #'call-next-method
                      self
                      url
                      :keep-alive t
                      :close nil
                      :stream (connection-stream connection)
                      args)))
            (t
             (log:debug "Creating a new stream")
             (apply #'call-next-method
                    self
                    url
                    :keep-alive t
                    :close nil
                    args)))
        (cond
          (should-close
           (warn "Server is making us close the stream")
           (values stream code headers))
          (t
           (unless reuse-context
             (error "Could not find reuse-context in ~s"
                    *reuse-contexts*))
           (prog1
               (values
                (cond
                  (want-stream
                   (make-instance 'tracked-stream
                                  :reuse-context reuse-context
                                  :bytes-left (parse-integer (assoc-value headers :content-length))
                                  :reusable-stream underlying-stream
                                  :domain (tracked-stream-key uri)
                                  :delegate stream))
                  (t
                   (push-connection reuse-context
                                (tracked-stream-key uri)
                                underlying-stream)
                   ;; will be an object!
                   stream))
                code
                headers)))))))))

(defun tracked-stream-key (uri)
  (format nil "~a://~a"
          (string-downcase (puri:uri-scheme uri))
          (puri:uri-host uri)))

#+nil
(hcl:profile
 (with-reused-ssl (*default-reused-ssl-engine*)
   (time
    (loop for i from 0 to 10
          collect (http-request
                   "http://ident.me"
                   :want-string t
                   :engine *default-reused-ssl-engine*)))))

#+nil
(hcl:profile
 (with-reused-ssl (*default-reused-ssl-engine*)
   (time
    (loop for i from 0 to 10
          collect (uiop:slurp-input-stream
                   'string
                   (http-request
                    "http://ident.me"
                    :want-stream t
                    :engine *default-reused-ssl-engine*))))))
