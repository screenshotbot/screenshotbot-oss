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
  (:import-from #:nursery
                #:nursery-let)
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
  ((lock :initform (bt:make-lock "reuse-context")
         :reader lock)
   (connections :initarg :connections
                :accessor connections
                :initform (make-hash-table :test #'equal)
                :documentation "Maps a domain to a stack of idle connections. A connection is only
in here while nobody is using it, so popping it checks it out for the
exclusive use of the caller, until it's pushed back. Only access this
with LOCK held.")
   (closedp :initform nil
            :accessor closedp
            :documentation "Set once the WITH-REUSED-SSL has exited. After that a connection that
gets returned is closed instead of being kept. Only access this with
LOCK held."))
  (:documentation "A pool of idle connections, which is safe to share between threads.
We never hold the lock while doing I/O."))

(defun close-connections (connections)
  (dolist (connection connections)
    (log:debug "Closing: ~a" (connection-stream connection))
    (close (connection-stream connection))))

(defmethod find-connection ((self reuse-context) domain)
  (trim-old-connections self)
  (bt:with-lock-held ((lock self))
    (pop (gethash domain (connections self)))))

(defmethod trim-old-connections ((self reuse-context))
  "Removes the connections that have been idle for longer than *TIMEOUT*,
and closes them."
  (let ((cutoff (- (get-universal-time) *timeout*)))
    (flet ((oldp (connection)
             (< (last-use-time connection) cutoff)))
      (close-connections
       (bt:with-lock-held ((lock self))
         (loop for domain being the hash-keys of (connections self)
                 using (hash-value connections)
               append (remove-if-not #'oldp connections)
               do (setf (gethash domain (connections self))
                        (remove-if #'oldp connections))))))))

(defmethod find-connection ((self null) domain)
  nil)

(defmethod %cleanup ((self reuse-context))
  (close-connections
   (bt:with-lock-held ((lock self))
     (setf (closedp self) t)
     (prog1
         (loop for connections being the hash-values of (connections self)
               append connections)
       (clrhash (connections self))))))

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
  "Requests made with ENGINE in the body reuse connections to the same
domain, and the connections are closed when this exits.

The connections are shared with the tasks of any nursery that's started
in the body, so put this around the WITH-NURSERY rather than inside each
task. Then the WITH-NURSERY waits for the tasks before we close the
connections."
  (let ((reuse-context (make-instance 'reuse-context)))
    (unwind-protect
         (nursery-let ((*reuse-contexts*
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
  "Returns STREAM to the pool, once nobody is using it anymore."
  (unless (bt:with-lock-held ((lock reuse-context))
            (unless (closedp reuse-context)
              (push
               (make-instance 'connection
                              :domain domain
                              :stream stream)
               (gethash domain (connections reuse-context)))
              t))
    ;; The WITH-REUSED-SSL has already exited (e.g. a stream from
    ;; :WANT-STREAM was closed afterwards), so nobody can reuse this.
    (close stream)))

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
  (let ((reuse-context (assoc-value *reuse-contexts* self)))
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
            ((find-connection reuse-context
                              (tracked-stream-key
                               (puri:uri url)))
             (let* ((connection anaphora:it))
               (log:debug "Reusing an existing stream ~a" (connection-stream connection))
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
            headers))))))))

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
                   "https://screenshotbot.io/api/version"
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
