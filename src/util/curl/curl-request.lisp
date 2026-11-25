;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;;; The libcurl implementation of CURL-ENGINE. This file is only loaded
;;;; on LispWorks on Linux and Mac, and uses the LispWorks FLI directly.
;;;;
;;;; Every transfer goes through a multi handle, even when we're
;;;; reading the whole body, so that :WANT-STREAM can return as soon as
;;;; the headers are in and then pull the body as it's read. Each
;;;; pooled CURL-HANDLE owns an easy handle and a multi handle, and the
;;;; multi handle owns the connection cache, so returning a handle to
;;;; the pool is what lets the next request reuse the connection.
;;;;
;;;; All callbacks run synchronously inside curl_multi_perform, on the
;;;; thread that called it, so they find their transfer in *TRANSFER*.

(in-package :util/curl/curl-request)

;;; Constants from curl/curl.h and curl/multi.h

(defconstant +curl-global-all+ 3)

(defconstant +curlopt-writedata+ 10001)
(defconstant +curlopt-url+ 10002)
(defconstant +curlopt-proxy+ 10004)
(defconstant +curlopt-errorbuffer+ 10010)
(defconstant +curlopt-writefunction+ 20011)
(defconstant +curlopt-readfunction+ 20012)
(defconstant +curlopt-httpheader+ 10023)
(defconstant +curlopt-customrequest+ 10036)
(defconstant +curlopt-nobody+ 44)
(defconstant +curlopt-upload+ 46)
(defconstant +curlopt-post+ 47)
(defconstant +curlopt-followlocation+ 52)
(defconstant +curlopt-autoreferer+ 58)
(defconstant +curlopt-ssl-verifypeer+ 64)
(defconstant +curlopt-maxredirs+ 68)
(defconstant +curlopt-headerfunction+ 20079)
(defconstant +curlopt-httpget+ 80)
(defconstant +curlopt-ssl-verifyhost+ 81)
(defconstant +curlopt-nosignal+ 99)
(defconstant +curlopt-accept-encoding+ 10102)
(defconstant +curlopt-infilesize-large+ 30115)
(defconstant +curlopt-postfieldsize-large+ 30120)
(defconstant +curlopt-connecttimeout-ms+ 156)
(defconstant +curlopt-http-content-decoding+ 158)
(defconstant +curlopt-postredir+ 161)
(defconstant +curlopt-seekfunction+ 20167)
(defconstant +curlopt-redir-protocols+ 182)
(defconstant +curlopt-suppress-connect-headers+ 265)
(defconstant +curlopt-prereqfunction+ 20312)

(defconstant +curlinfo-effective-url+ #x100001)

(defconstant +curlmopt-maxconnects+ 6)

(defconstant +curlproto-http+ 1)
(defconstant +curlproto-https+ 2)
(defconstant +curl-redir-post-301+ 1)

(defconstant +curl-readfunc-abort+ #x10000000)
(defconstant +curl-seekfunc-ok+ 0)
(defconstant +curl-seekfunc-cantseek+ 2)

(defconstant +curlmsg-done+ 1)
(defconstant +curl-error-size+ 256)

(defparameter *connection-error-codes*
  '(5  ;; COULDNT_RESOLVE_PROXY
    6  ;; COULDNT_RESOLVE_HOST
    7  ;; COULDNT_CONNECT
    35 ;; SSL_CONNECT_ERROR
    53 ;; SSL_ENGINE_NOTFOUND
    54 ;; SSL_ENGINE_SETFAILED
    58 ;; SSL_CERTPROBLEM
    59 ;; SSL_CIPHER
    60 ;; PEER_FAILED_VERIFICATION
    64 ;; USE_SSL_FAILED
    66 ;; SSL_ENGINE_INITFAILED
    77 ;; SSL_CACERT_BADFILE
    83 ;; SSL_ISSUER_ERROR
    90 ;; SSL_PINNEDPUBKEYNOTMATCH
    91 ;; SSL_INVALIDCERTSTATUS
    98 ;; SSL_CLIENTCERT
    )
  "CURLcodes that mean we never got to talk HTTP with the server. These
correspond to the LispWorks socket-create and SSL errors that
WRAP-SSL-ERRORS turns into a 502.")

(defconstant +curle-operation-timedout+ 28)

;;; Loading the library

(defparameter *libcurl-names*
  #+darwin '("libcurl.4.dylib" "libcurl.dylib")
  #-darwin '("libcurl.so.4" "libcurl.so" "libcurl-gnutls.so.4"))

(defvar *libcurl-state* nil
  "NIL if we haven't tried to load libcurl yet in this image, otherwise
:LOADED or :FAILED.")

(defvar *libcurl-lock* (bt:make-lock "libcurl"))

(defun %load-libcurl ()
  (loop for name in *libcurl-names*
        do (handler-case
               (progn
                 (fli:register-module :libcurl
                                      :real-name name
                                      :connection-style :immediate)
                 (let ((code (curl-global-init +curl-global-all+)))
                   (unless (eql 0 code)
                     (error "curl_global_init failed with ~a" code)))
                 (log:debug "Loaded ~a" (curl-version-string))
                 (return :loaded))
             (error (e)
               (log:debug "Could not load ~a: ~a" name e)))
        finally
           (log:warn "Could not load libcurl (tried ~{~a~^, ~}), falling back to Drakma"
                     *libcurl-names*)
           (return :failed)))

(defun libcurl-available-p ()
  "Loads libcurl if it hasn't been loaded in this image yet. Returns NIL if
it can't be loaded, in which case CURL-ENGINE uses Drakma."
  (and
   *libcurl-enabled-p*
   (eql :loaded
        (or *libcurl-state*
            (bt:with-lock-held (*libcurl-lock*)
              (or *libcurl-state*
                  (setf *libcurl-state* (%load-libcurl))))))))

;;; FLI bindings

(fli:define-c-struct curl-msg
  (msg :int)
  (easy-handle :pointer)
  ;; data is a union of void* and CURLcode, the CURLcode is at its
  ;; start.
  (result :int))

(fli:define-foreign-function (curl-global-init "curl_global_init" :source)
    ((flags :long))
  :result-type :int
  :module :libcurl)

(fli:define-foreign-function (%curl-version "curl_version" :source)
    ()
  :result-type (:pointer :char)
  :module :libcurl)

(defun curl-version-string ()
  (fli:convert-from-foreign-string (%curl-version)))

(fli:define-foreign-function (curl-easy-init "curl_easy_init" :source)
    ()
  :result-type :pointer
  :module :libcurl)

(fli:define-foreign-function (curl-easy-cleanup "curl_easy_cleanup" :source)
    ((handle :pointer))
  :result-type :void
  :module :libcurl)

(fli:define-foreign-function (curl-easy-reset "curl_easy_reset" :source)
    ((handle :pointer))
  :result-type :void
  :module :libcurl)

(fli:define-foreign-function (%curl-easy-strerror "curl_easy_strerror" :source)
    ((code :int))
  :result-type (:pointer :char)
  :module :libcurl)

;; curl_easy_setopt and friends are variadic, which matters on arm64
;; Macs where variadic arguments go on the stack.
(fli:define-foreign-function (%setopt-long "curl_easy_setopt" :source)
    ((handle :pointer)
     (option :int)
     (value :long))
  :result-type :int
  :variadic-num-of-fixed 2
  :module :libcurl)

(fli:define-foreign-function (%setopt-pointer "curl_easy_setopt" :source)
    ((handle :pointer)
     (option :int)
     (value :pointer))
  :result-type :int
  :variadic-num-of-fixed 2
  :module :libcurl)

(fli:define-foreign-function (%setopt-off "curl_easy_setopt" :source)
    ((handle :pointer)
     (option :int)
     (value :int64))
  :result-type :int
  :variadic-num-of-fixed 2
  :module :libcurl)

(fli:define-foreign-function (%getinfo-pointer "curl_easy_getinfo" :source)
    ((handle :pointer)
     (info :int)
     (out :pointer))
  :result-type :int
  :variadic-num-of-fixed 2
  :module :libcurl)

(fli:define-foreign-function (curl-multi-init "curl_multi_init" :source)
    ()
  :result-type :pointer
  :module :libcurl)

(fli:define-foreign-function (curl-multi-cleanup "curl_multi_cleanup" :source)
    ((multi :pointer))
  :result-type :int
  :module :libcurl)

(fli:define-foreign-function (%multi-setopt-long "curl_multi_setopt" :source)
    ((multi :pointer)
     (option :int)
     (value :long))
  :result-type :int
  :variadic-num-of-fixed 2
  :module :libcurl)

(fli:define-foreign-function (curl-multi-add-handle "curl_multi_add_handle" :source)
    ((multi :pointer)
     (easy :pointer))
  :result-type :int
  :module :libcurl)

(fli:define-foreign-function (curl-multi-remove-handle "curl_multi_remove_handle" :source)
    ((multi :pointer)
     (easy :pointer))
  :result-type :int
  :module :libcurl)

(fli:define-foreign-function (curl-multi-perform "curl_multi_perform" :source)
    ((multi :pointer)
     (running-handles (:pointer :int)))
  :result-type :int
  :module :libcurl)

(fli:define-foreign-function (curl-multi-poll "curl_multi_poll" :source)
    ((multi :pointer)
     (extra-fds :pointer)
     (extra-nfds :unsigned-int)
     (timeout-ms :int)
     (numfds (:pointer :int)))
  :result-type :int
  :module :libcurl)

(fli:define-foreign-function (curl-multi-info-read "curl_multi_info_read" :source)
    ((multi :pointer)
     (msgs-in-queue (:pointer :int)))
  :result-type (:pointer (:struct curl-msg))
  :module :libcurl)

(fli:define-foreign-function (%curl-multi-strerror "curl_multi_strerror" :source)
    ((code :int))
  :result-type (:pointer :char)
  :module :libcurl)

(fli:define-foreign-function (%curl-slist-append "curl_slist_append" :source)
    ((list :pointer)
     (string :pointer))
  :result-type :pointer
  :module :libcurl)

(fli:define-foreign-function (curl-slist-free-all "curl_slist_free_all" :source)
    ((list :pointer))
  :result-type :void
  :module :libcurl)

(defun curl-easy-strerror (code)
  (fli:convert-from-foreign-string (%curl-easy-strerror code)))

(defun check-setopt (code option)
  (unless (eql 0 code)
    (error 'curl-error
           :code code
           :message (format nil "curl_easy_setopt(~a): ~a"
                            option (curl-easy-strerror code)))))

(defun setopt-long (easy option value)
  (check-setopt (%setopt-long easy option value) option))

(defun setopt-pointer (easy option value)
  (check-setopt (%setopt-pointer easy option value) option))

(defun setopt-off (easy option value)
  (check-setopt (%setopt-off easy option value) option))

(defun setopt-string (easy option value)
  "libcurl copies string options, so the foreign string only needs to
live for the duration of the call."
  (fli:with-foreign-string (ptr elements bytes :external-format :utf-8)
      value
    (declare (ignore elements bytes))
    (setopt-pointer easy option ptr)))

(defun slist-append (slist string)
  (fli:with-foreign-string (ptr elements bytes :external-format :latin-1)
      string
    (declare (ignore elements bytes))
    (let ((result (%curl-slist-append slist ptr)))
      (when (fli:null-pointer-p result)
        (error "curl_slist_append failed"))
      result)))

(defun check-multi-code (code)
  (unless (eql 0 code)
    (error 'curl-error
           :code code
           :message (fli:convert-from-foreign-string (%curl-multi-strerror code)))))

;;; Handles and the handle pool

(defclass curl-handle ()
  ((easy :initarg :easy
         :reader easy)
   (multi :initarg :multi
          :reader multi)
   (error-buffer :initarg :error-buffer
                 :reader error-buffer)
   (scratch :initform (make-array 16384 :element-type '(unsigned-byte 8))
            :reader scratch
            :documentation "Used by the read callback to stage the upload data."))
  (:documentation "An easy handle together with the multi handle that drives it. The
multi handle owns the connection cache, so these get pooled, and a
CURL-HANDLE is only ever used by one transfer at a time."))

(defvar *pool* nil
  "Idle CURL-HANDLEs. Only access this with *POOL-LOCK* held.")

(defvar *pool-lock* (bt:make-lock "curl-pool"))

(defparameter *max-idle-handles* 16)

(defparameter *max-connects-per-handle* 8)

(defun make-curl-handle ()
  (let ((easy (curl-easy-init))
        (multi (curl-multi-init)))
    (when (or (fli:null-pointer-p easy)
              (fli:null-pointer-p multi))
      (error 'curl-error :message "Could not create a curl handle"))
    (%multi-setopt-long multi +curlmopt-maxconnects+ *max-connects-per-handle*)
    (make-instance 'curl-handle
                   :easy easy
                   :multi multi
                   :error-buffer (fli:allocate-foreign-object
                                  :type :char
                                  :nelems +curl-error-size+))))

(defun destroy-curl-handle (handle)
  (curl-easy-cleanup (easy handle))
  ;; This closes the connections in the cache
  (curl-multi-cleanup (multi handle))
  (fli:free-foreign-object (error-buffer handle)))

(defun checkout-handle ()
  (let ((handle (or (bt:with-lock-held (*pool-lock*)
                      (pop *pool*))
                    (make-curl-handle))))
    (curl-easy-reset (easy handle))
    (setf (fli:dereference (error-buffer handle) :index 0) 0)
    handle))

(defun checkin-handle (handle)
  (unless (bt:with-lock-held (*pool-lock*)
            (when (< (length *pool*) *max-idle-handles*)
              (push handle *pool*)
              t))
    (destroy-curl-handle handle)))

(defun clear-pool ()
  "Closes all the idle connections."
  (mapc #'destroy-curl-handle
        (bt:with-lock-held (*pool-lock*)
          (shiftf *pool* nil))))

(defun reset-for-new-image ()
  ;; Foreign pointers from a saved image are meaningless now, and the
  ;; module has to be registered again.
  (setf *pool* nil)
  (setf *libcurl-state* nil))

(lw:define-action "When starting image" "Reset libcurl state for util/curl/curl-request"
  'reset-for-new-image)

;;; Transfers

(defvar *transfer* nil
  "The TRANSFER being driven by curl_multi_perform, for the callbacks.")

(defclass transfer ()
  ((handle :initarg :handle
           :reader handle)
   (url :initarg :url
        :reader transfer-url)
   (follow-redirects-p :initarg :follow-redirects-p
                       :reader follow-redirects-p)
   (redirects-left :initarg :redirects-left
                   :accessor redirects-left
                   :documentation "How many more redirects libcurl will follow, or NIL if unlimited")
   (current-url :initarg :url
                :accessor current-url
                :documentation "The URL of the response we're currently reading, as a PURI:URI
or string, for resolving relative redirects.")
   (status :initform nil
           :accessor status)
   (reason :initform nil
           :accessor reason)
   (raw-headers :initform nil
                :accessor raw-headers
                :documentation "Reversed list of (name . value) of the current response")
   (headers-done-p :initform nil
                   :accessor headers-done-p
                   :documentation "Set once we have all the headers of the final response")
   (incoming :initform nil
             :accessor incoming
             :documentation "Reversed list of body chunks that haven't been read yet")
   (outgoing :initform nil
             :accessor outgoing
             :documentation "Body chunks that haven't been read yet, in order")
   (current :initform nil
            :accessor current)
   (current-pos :initform 0
                :accessor current-pos)
   (done-p :initform nil
           :accessor done-p)
   (result :initform nil
           :accessor result
           :documentation "The CURLcode once the transfer is done")
   (callback-error :initform nil
                   :accessor callback-error
                   :documentation "A Lisp error that happened in a callback, which made us abort the transfer")
   (poisoned-p :initform nil
               :accessor poisoned-p
               :documentation "Set if we unwound through libcurl, in which case we don't trust
the handle anymore.")
   (upload :initarg :upload
           :initform nil
           :reader upload)
   (close-upload-p :initarg :close-upload-p
                   :initform nil
                   :reader close-upload-p)
   (slist :initform nil
          :accessor slist)
   (connection-timeout :initarg :connection-timeout
                       :reader connection-timeout)
   (idle-timeout :initarg :idle-timeout
                 :reader idle-timeout)
   (last-activity :initform (get-internal-real-time)
                  :accessor last-activity)
   (connectedp :initform nil
               :accessor connectedp
               :documentation "Set once we have a connection, after which the idle timeout
applies.")
   (attachedp :initform nil
              :accessor attachedp)
   (releasedp :initform nil
              :accessor releasedp)))

(defun touch (transfer)
  (setf (last-activity transfer) (get-internal-real-time)))

(defmacro with-callback-guard ((transfer error-value) &body body)
  "Callbacks are called from C, so we must not unwind through them. A
Lisp error aborts the transfer instead (by returning ERROR-VALUE) and
is re-signaled once curl_multi_perform returns."
  (a:with-gensyms (completedp)
    `(let ((,completedp nil))
       (unwind-protect
            (multiple-value-prog1
                (handler-case
                    (progn ,@body)
                  (error (e)
                    (setf (callback-error ,transfer) e)
                    ,error-value))
              (setf ,completedp t))
         (unless ,completedp
           ;; Something like a thread interrupt unwound through libcurl.
           (setf (poisoned-p ,transfer) t))))))

(defun redirect-location (transfer)
  "If libcurl is going to follow the current response, the Location it
redirects to."
  (let ((status (status transfer)))
    (and (follow-redirects-p transfer)
         (or (null (redirects-left transfer))
             (plusp (redirects-left transfer)))
         ;; libcurl follows any 3xx with a Location, not just the usual
         ;; redirect codes.
         (<= 300 status 399)
         (cdr (assoc "location" (raw-headers transfer)
                     :test #'string-equal)))))

(defun on-headers-done (transfer)
  "Called on the blank line after each response's headers."
  (let ((status (status transfer)))
    (when (and status (>= status 200))
      (let ((location (redirect-location transfer)))
        (cond
          (location
           (let ((new-uri (puri:merge-uris location (current-url transfer))))
             (when (redirects-left transfer)
               (decf (redirects-left transfer)))
             (setf (current-url transfer) new-uri)
             ;; The redirect might go to a new host
             (setf (connectedp transfer) nil)
             ;; Like Drakma, so that callers can validate the redirect.
             ;; If a handler signals an error, the callback guard aborts
             ;; the transfer.
             (signal 'drakma::http-redirecting :location new-uri)))
          (t
           (setf (headers-done-p transfer) t)))))))

(defun parse-status-line (transfer line)
  ;; "HTTP/1.1 200 OK" or "HTTP/2 200"
  (let* ((first-space (position #\Space line))
         (second-space (and first-space
                            (position #\Space line :start (1+ first-space)))))
    (setf (status transfer)
          (and first-space
               (parse-integer line :start (1+ first-space)
                                   :end second-space
                                   :junk-allowed t)))
    (setf (reason transfer)
          (if second-space
              (subseq line (1+ second-space))
              ""))
    (setf (raw-headers transfer) nil)
    (setf (headers-done-p transfer) nil)
    (setf (connectedp transfer) t)))

(defun on-header-line (transfer line)
  (let ((line (string-right-trim '(#\Return #\Newline) line)))
    (cond
      ((and (>= (length line) 5)
            (string= "HTTP/" line :end2 5))
       (parse-status-line transfer line))
      ((equal "" line)
       (on-headers-done transfer))
      ((member (char line 0) '(#\Space #\Tab))
       ;; obsolete line folding
       (let ((last (car (raw-headers transfer))))
         (when last
           (setf (cdr last)
                 (format nil "~a ~a" (cdr last) (string-trim '(#\Space #\Tab) line))))))
      (t
       (let ((pos (position #\: line)))
         (when pos
           (push (cons (subseq line 0 pos)
                       (string-trim '(#\Space #\Tab) (subseq line (1+ pos))))
                 (raw-headers transfer))))))))

(fli:define-foreign-callable ("util_curl_header_cb" :result-type :size-t)
    ((buffer (:pointer :char))
     (size :size-t)
     (nitems :size-t)
     (userdata :pointer))
  (declare (ignore userdata))
  (let ((transfer *transfer*)
        (n (* size nitems)))
    (with-callback-guard (transfer 0)
      (touch transfer)
      (on-header-line transfer
                      (fli:convert-from-foreign-string
                       buffer
                       :length n
                       :null-terminated-p nil
                       :external-format :latin-1))
      n)))

(fli:define-foreign-callable ("util_curl_write_cb" :result-type :size-t)
    ((buffer (:pointer (:unsigned :byte)))
     (size :size-t)
     (nmemb :size-t)
     (userdata :pointer))
  (declare (ignore userdata))
  (let ((transfer *transfer*)
        (n (* size nmemb)))
    (with-callback-guard (transfer 0)
      (touch transfer)
      (when (> n 0)
        (let ((chunk (make-array n :element-type '(unsigned-byte 8))))
          (fli:replace-foreign-array chunk buffer :end1 n :end2 n)
          (push chunk (incoming transfer))))
      n)))

(fli:define-foreign-callable ("util_curl_read_cb" :result-type :size-t)
    ((buffer (:pointer (:unsigned :byte)))
     (size :size-t)
     (nitems :size-t)
     (userdata :pointer))
  (declare (ignore userdata))
  (let ((transfer *transfer*))
    (with-callback-guard (transfer +curl-readfunc-abort+)
      (touch transfer)
      (let* ((scratch (scratch (handle transfer)))
             (count (read-sequence scratch (upload transfer)
                                   :end (min (* size nitems)
                                             (length scratch)))))
        (when (> count 0)
          (fli:replace-foreign-array buffer scratch :end1 count :end2 count))
        count))))

(fli:define-foreign-callable ("util_curl_prereq_cb" :result-type :int)
    ((clientp :pointer)
     (primary-ip :pointer)
     (local-ip :pointer)
     (primary-port :int)
     (local-port :int))
  (declare (ignore clientp primary-ip local-ip primary-port local-port))
  ;; Called once we have a connection (new or reused), right before the
  ;; request is sent.
  (let ((transfer *transfer*))
    (with-callback-guard (transfer 0)
      (touch transfer)
      (setf (connectedp transfer) t)
      0)))

(fli:define-foreign-callable ("util_curl_seek_cb" :result-type :int)
    ((userdata :pointer)
     (offset :int64)
     (origin :int))
  (declare (ignore userdata))
  ;; libcurl only seeks to rewind the upload, e.g. on a redirect.
  (let ((transfer *transfer*))
    (with-callback-guard (transfer +curl-seekfunc-cantseek+)
      (if (and (eql origin 0)
               (file-position (upload transfer) offset))
          +curl-seekfunc-ok+
          +curl-seekfunc-cantseek+))))

(defun read-messages (transfer)
  (let ((multi (multi (handle transfer))))
    (fli:with-dynamic-foreign-objects ((queued :int))
      (loop for msg = (curl-multi-info-read multi queued)
            until (fli:null-pointer-p msg)
            do (when (eql +curlmsg-done+ (fli:foreign-slot-value msg 'msg))
                 (setf (result transfer) (fli:foreign-slot-value msg 'result))
                 (setf (done-p transfer) t))))))

(defun error-message (transfer code)
  (let ((detail (fli:convert-from-foreign-string
                 (error-buffer (handle transfer))
                 :external-format :latin-1)))
    (if (equal "" detail)
        (curl-easy-strerror code)
        detail)))

(defun signal-transfer-error (transfer)
  (a:when-let ((e (callback-error transfer)))
    (error e))
  (let ((code (result transfer)))
    (unless (eql 0 code)
      (error (cond
               ((eql code +curle-operation-timedout+)
                (if (connectedp transfer)
                    'curl-timeout-error
                    'curl-connect-timeout-error))
               ((member code *connection-error-codes*)
                'curl-connection-error)
               (t
                'curl-error))
             :code code
             :url (transfer-url transfer)
             :message (error-message transfer code)))))

(defun check-idle (transfer)
  (let ((limit (idle-timeout transfer)))
    (when limit
      (let ((limit (if (connectedp transfer)
                       limit
                       ;; We might still be connecting, which has its own
                       ;; timeout.
                       (+ limit (or (connection-timeout transfer) 0))))
            (idle (/ (- (get-internal-real-time) (last-activity transfer))
                     internal-time-units-per-second)))
        (when (> idle limit)
          (error (if (connectedp transfer)
                     'curl-timeout-error
                     'curl-connect-timeout-error)
                 :url (transfer-url transfer)
                 :message (format nil "No data for ~a seconds" limit)))))))

(defun drive (transfer until)
  "Runs the transfer until it's done, or (FUNCALL UNTIL) is true."
  (let ((*transfer* transfer)
        (multi (multi (handle transfer))))
    ;; Time when the caller wasn't reading (with :WANT-STREAM) doesn't
    ;; count towards the timeout.
    (touch transfer)
    (fli:with-dynamic-foreign-objects ((running :int)
                                       (numfds :int))
      (loop
        ;; Don't let an interrupt (e.g. a thread being killed) unwind
        ;; through libcurl from inside a callback: that leaves the multi
        ;; handle unusable, and we'd have to leak it and its
        ;; connections. Callbacks only block when reading the upload
        ;; stream, which delays the interrupt until that read returns.
        (check-multi-code (mp:with-interrupts-blocked
                            (curl-multi-perform multi running)))
        (read-messages transfer)
        (when (callback-error transfer)
          (signal-transfer-error transfer))
        (when (or (done-p transfer)
                  (funcall until))
          (return))
        (check-idle transfer)
        (check-multi-code
         (curl-multi-poll multi fli:*null-pointer* 0 250 numfds))))
    (when (done-p transfer)
      (signal-transfer-error transfer))))

(defun release-transfer (transfer)
  (unless (releasedp transfer)
    (setf (releasedp transfer) t)
    (let ((handle (handle transfer)))
      (unwind-protect
           (progn
             (when (and (attachedp transfer)
                        ;; libcurl still thinks it's inside a callback,
                        ;; so it would refuse this anyway.
                        (not (poisoned-p transfer)))
               ;; If the transfer isn't done, this aborts it and closes
               ;; its connection.
               (curl-multi-remove-handle (multi handle) (easy handle)))
             (when (slist transfer)
               (curl-slist-free-all (slist transfer)))
             (when (close-upload-p transfer)
               (close (upload transfer))))
        (cond
          ((poisoned-p transfer)
           ;; libcurl refuses every call on a multi handle that we
           ;; unwound out of, so there's no safe way to free it or its
           ;; connections. DRIVE blocks interrupts to avoid this.
           (log:error "Leaking a curl handle that we unwound through"))
          (t
           (checkin-handle handle)))))))

(defun next-chunk (transfer)
  "Returns the next unread body chunk, or NIL if there isn't one yet."
  (unless (outgoing transfer)
    (setf (outgoing transfer)
          (nreverse (shiftf (incoming transfer) nil))))
  (pop (outgoing transfer)))

(defun ensure-current-chunk (transfer)
  "Makes sure (CURRENT TRANSFER) has unread bytes, driving the transfer if
needed. Returns NIL at EOF."
  (loop
    (let ((current (current transfer)))
      (when (and current
                 (< (current-pos transfer) (length current)))
        (return t)))
    (let ((next (next-chunk transfer)))
      (cond
        (next
         (setf (current transfer) next)
         (setf (current-pos transfer) 0))
        ((done-p transfer)
         (setf (current transfer) nil)
         (return nil))
        (t
         (drive transfer
                (lambda ()
                  (or (incoming transfer)
                      (outgoing transfer)))))))))

(defun all-body-octets (transfer)
  (let* ((chunks (append (when (current transfer)
                           (list (subseq (current transfer) (current-pos transfer))))
                         (outgoing transfer)
                         (reverse (incoming transfer))))
         (result (make-array (reduce #'+ chunks :key #'length)
                             :element-type '(unsigned-byte 8))))
    (loop with pos = 0
          for chunk in chunks
          do (replace result chunk :start1 pos)
             (incf pos (length chunk)))
    result))

;;; The stream returned for :WANT-STREAM

(defclass curl-input-stream (stream:fundamental-binary-input-stream)
  ((transfer :initarg :transfer
             :reader transfer)
   (closedp :initform nil
            :accessor closedp))
  (:documentation "The body of a response, read from libcurl as it's needed. Closing
this returns the handle to the pool, which lets the connection be
reused if we got to the end of the body."))

(defmethod stream-element-type ((self curl-input-stream))
  '(unsigned-byte 8))

(defun check-open (self)
  (when (closedp self)
    (error "The curl stream is already closed")))

(defmethod stream:stream-read-byte ((self curl-input-stream))
  (check-open self)
  (let ((transfer (transfer self)))
    (cond
      ((ensure-current-chunk transfer)
       (prog1
           (aref (current transfer) (current-pos transfer))
         (incf (current-pos transfer))))
      (t
       :eof))))

(defmethod stream:stream-read-sequence ((self curl-input-stream)
                                        sequence
                                        start
                                        end)
  (check-open self)
  (let ((transfer (transfer self))
        (pos start))
    (loop while (and (< pos end)
                     (ensure-current-chunk transfer))
          do (let* ((current (current transfer))
                    (count (min (- end pos)
                                (- (length current) (current-pos transfer)))))
               (replace sequence current
                        :start1 pos
                        :end1 (+ pos count)
                        :start2 (current-pos transfer))
               (incf pos count)
               (incf (current-pos transfer) count)))
    pos))

(defmethod stream:stream-listen ((self curl-input-stream))
  (let ((transfer (transfer self)))
    (or (and (current transfer)
             (< (current-pos transfer) (length (current transfer))))
        (incoming transfer)
        (outgoing transfer))))

(defmethod close ((self curl-input-stream) &key abort)
  (declare (ignore abort))
  (unless (closedp self)
    (setf (closedp self) t)
    (release-transfer (transfer self)))
  (call-next-method))

;;; Translating Drakma's arguments

(defparameter *supported-args*
  '(:method :parameters :url-encoder :content :content-type :content-length
    :form-data :basic-authorization :user-agent :accept :range :proxy
    :no-proxy-domains :additional-headers :redirect :auto-referer
    :keep-alive :close :external-format-out :external-format-in
    :force-binary :want-stream :decode-content :connection-timeout
    :read-timeout :write-timeout :verify :max-depth :protocol)
  "Arguments to DRAKMA:HTTP-REQUEST that we handle. If we're passed any
other argument with a non-NIL value, we fall back to Drakma.")

(defun file-parameters-p (parameters)
  (find-if-not (lambda (value)
                 (or (stringp value)
                     (null value)))
               parameters :key #'cdr))

(defun parse-uri (url)
  (cond
    ((typep url 'puri:uri)
     (puri:copy-uri url))
    ((typep url 'quri:uri)
     (puri:parse-uri (quri:render-uri url)))
    (t
     (puri:parse-uri url))))

(defun binary-input-stream-p (content)
  (and (streamp content)
       (input-stream-p content)
       (subtypep (stream-element-type content) '(unsigned-byte 8))))

(defun supported-request-p (url args)
  (destructuring-bind (&key (method :get) parameters content form-data
                         (protocol :http/1.1)
                       &allow-other-keys)
      args
    (and
     (loop for (key value) on args by #'cddr
           always (or (null value)
                      (member key *supported-args*)))
     (eql protocol :http/1.1)
     (not (eql method :options*))
     ;; Uploading files with multipart/form-data
     (not (file-parameters-p parameters))
     (not (and form-data content))
     (or (null content)
         (stringp content)
         (typep content '(vector (unsigned-byte 8)))
         (and (listp content) (every #'integerp content))
         (pathnamep content)
         (binary-input-stream-p content))
     (member (puri:uri-scheme (parse-uri url)) '(:http :https)))))

(defun header-name (name)
  (if (symbolp name)
      (string-capitalize (symbol-name name))
      name))

(defun header-present-p (name headers)
  (find name headers :key (lambda (header)
                            (header-name (car header)))
                     :test #'string-equal))

(defun header-value-string (value)
  ;; Drakma allows functions as header values
  (format nil "~a"
          (cond ((or (functionp value)
                     (and (symbolp value)
                          value
                          (fboundp value)))
                 (funcall value))
                (t value))))

(defun build-request-uri (uri parameters parameters-used-p external-format-out url-encoder)
  "Mirrors how Drakma merges PARAMETERS into the query string."
  (a:when-let ((all-get-parameters
                (append (drakma::dissect-query (puri:uri-query uri))
                        (and (not parameters-used-p) parameters))))
    (setf (puri:uri-query uri)
          (drakma::alist-to-url-encoded-string all-get-parameters
                                               external-format-out
                                               url-encoder)))
  (puri:render-uri uri nil))

(defun content-octets (content external-format-out)
  (etypecase content
    (string
     (flex:string-to-octets content :external-format external-format-out))
    ((vector (unsigned-byte 8))
     content)
    (list
     (coerce content '(vector (unsigned-byte 8))))))

(defun form-data-octets (parameters external-format-out)
  "Returns the multipart/form-data body, and its content-type."
  (let* ((boundary (format nil "----------~A" (drakma::make-random-string)))
         (fn (drakma::make-form-data-function parameters boundary external-format-out)))
    (values
     (flex:with-output-to-sequence (out)
       (funcall fn (flex:make-flexi-stream out :external-format drakma::+latin-1+)))
     (format nil "multipart/form-data; boundary=~A" boundary))))

(defun headers-alist (transfer)
  "The headers like Drakma returns them: keyword names where there's a
keyword (otherwise strings), and repeated headers joined with commas."
  (let ((result nil))
    (loop for (name . value) in (reverse (raw-headers transfer))
          for key = (chunga::as-keyword-if-found name)
          for existing = (assoc key result :test #'equal)
          do (cond
               (existing
                (setf (cdr existing)
                      (format nil "~a,~a" (cdr existing) value)))
               (t
                (push (cons key value) result))))
    (nreverse result)))

(defun effective-uri (transfer)
  (fli:with-dynamic-foreign-objects ((out :pointer))
    (let ((ptr (and (eql 0 (%getinfo-pointer (easy (handle transfer))
                                             +curlinfo-effective-url+
                                             out))
                    (fli:dereference out))))
      (puri:parse-uri
       (if (and ptr (not (fli:null-pointer-p ptr)))
           (fli:convert-from-foreign-string ptr :external-format :latin-1)
           (transfer-url transfer))))))

(defun configure-handle (transfer &key easy url method upload upload-size
                                    headers redirect auto-referer proxy
                                    verify decode-content connection-timeout)
  (setopt-string easy +curlopt-url+ url)
  (setopt-long easy +curlopt-nosignal+ 1)
  (setopt-pointer easy +curlopt-errorbuffer+ (error-buffer (handle transfer)))
  (setopt-pointer easy +curlopt-writefunction+
                  (fli:make-pointer :symbol-name "util_curl_write_cb"))
  (setopt-pointer easy +curlopt-headerfunction+
                  (fli:make-pointer :symbol-name "util_curl_header_cb"))
  (setopt-long easy +curlopt-suppress-connect-headers+ 1)
  ;; Needs libcurl 7.80. Without it the idle timeout only starts once we
  ;; get a response, so a slow server takes connection-timeout longer
  ;; to time out.
  (unless (eql 0 (%setopt-pointer easy +curlopt-prereqfunction+
                                  (fli:make-pointer :symbol-name "util_curl_prereq_cb")))
    (log:debug "CURLOPT_PREREQFUNCTION is not supported by this libcurl"))
  (setopt-long easy +curlopt-redir-protocols+
               (logior +curlproto-http+ +curlproto-https+))

  (cond
    (redirect
     (setopt-long easy +curlopt-followlocation+ 1)
     (setopt-long easy +curlopt-maxredirs+ (if (integerp redirect) redirect -1))
     ;; Like Drakma, only 302 and 303 turn a POST into a GET. (Drakma
     ;; does keep the POST on a 301.)
     (setopt-long easy +curlopt-postredir+ +curl-redir-post-301+)
     (when auto-referer
       (setopt-long easy +curlopt-autoreferer+ 1)))
    (t
     (setopt-long easy +curlopt-followlocation+ 0)))

  (when connection-timeout
    (setopt-long easy +curlopt-connecttimeout-ms+
                 (round (* 1000 connection-timeout))))

  (cond
    (verify
     (setopt-long easy +curlopt-ssl-verifypeer+ 1)
     (setopt-long easy +curlopt-ssl-verifyhost+ 2))
    (t
     (setopt-long easy +curlopt-ssl-verifypeer+ 0)
     (setopt-long easy +curlopt-ssl-verifyhost+ 0)))

  (when proxy
    (destructuring-bind (host &optional (port 80)) (a:ensure-list proxy)
      (setopt-string easy +curlopt-proxy+ (format nil "http://~a:~a" host port))))

  (cond
    (decode-content
     ;; An empty string enables all the encodings libcurl supports. We
     ;; remove the Accept-Encoding header that this adds unless the
     ;; caller asked for one, so that we behave like Drakma.
     (setopt-string easy +curlopt-accept-encoding+ "")
     (setopt-long easy +curlopt-http-content-decoding+ 1))
    (t
     (setopt-long easy +curlopt-http-content-decoding+ 0)))

  (when upload
    (setopt-pointer easy +curlopt-readfunction+
                    (fli:make-pointer :symbol-name "util_curl_read_cb"))
    (setopt-pointer easy +curlopt-seekfunction+
                    (fli:make-pointer :symbol-name "util_curl_seek_cb")))

  (let ((method-name (string-upcase (symbol-name method))))
    (case method
      (:head
       (setopt-long easy +curlopt-nobody+ 1))
      (:put
       (setopt-long easy +curlopt-upload+ 1)
       (setopt-off easy +curlopt-infilesize-large+
                   (if upload (or upload-size -1) 0)))
      (t
       (cond
         ((or upload (eql method :post))
          (setopt-long easy +curlopt-post+ 1)
          (setopt-off easy +curlopt-postfieldsize-large+
                      (if upload (or upload-size -1) 0))
          (unless (eql method :post)
            (setopt-string easy +curlopt-customrequest+ method-name)))
         ((eql method :get)
          (setopt-long easy +curlopt-httpget+ 1))
         (t
          (setopt-string easy +curlopt-customrequest+ method-name))))))

  (let ((slist fli:*null-pointer*))
    (dolist (header headers)
      (setf slist (slist-append slist header))
      (setf (slist transfer) slist))
    (setopt-pointer easy +curlopt-httpheader+ slist)))

(defun curl-request (url &key (method :get)
                           parameters
                           (url-encoder #'drakma::url-encode)
                           content
                           (content-type "application/x-www-form-urlencoded")
                           (content-length nil content-length-provided-p)
                           form-data
                           basic-authorization
                           (user-agent :drakma)
                           (accept "*/*")
                           range
                           (proxy drakma:*default-http-proxy*)
                           (no-proxy-domains drakma:*no-proxy-domains*)
                           additional-headers
                           (redirect 5)
                           auto-referer
                           (external-format-out drakma:*drakma-default-external-format*)
                           (external-format-in drakma:*drakma-default-external-format*)
                           force-binary
                           want-stream
                           decode-content
                           (connection-timeout 20)
                           (read-timeout 20)
                           verify
                         &allow-other-keys)
  "Does what DRAKMA:HTTP-REQUEST would do with these arguments, using
libcurl, and returns the same values."
  (let ((uri (parse-uri url))
        (parameters-used-p nil)
        (upload nil)
        (upload-size nil)
        (close-upload-p nil))
    (when (member (puri:uri-host uri) no-proxy-domains :test #'string=)
      (setf proxy nil))
    (when (and (eql method :post)
               (not content))
      (setf parameters-used-p t)
      (cond
        (form-data
         (multiple-value-setq (content content-type)
           (form-data-octets parameters external-format-out)))
        (t
         (setf content (drakma::alist-to-url-encoded-string
                        parameters external-format-out url-encoder))
         (setf content-type "application/x-www-form-urlencoded"))))

    (when content
      (cond
        ((pathnamep content)
         (setf upload (open content :element-type '(unsigned-byte 8)))
         (setf close-upload-p t)
         (setf upload-size (if (integerp content-length)
                               content-length
                               (file-length upload))))
        ((streamp content)
         (cond
           ((integerp content-length)
            (setf upload content)
            (setf upload-size content-length))
           ((and content-length-provided-p content-length)
            ;; Drakma reads the stream into memory to find the
            ;; Content-Length
            (let ((octets (flex:with-output-to-sequence (out)
                            (uiop:copy-stream-to-stream content out
                                                        :element-type '(unsigned-byte 8)))))
              (setf upload (flex:make-in-memory-input-stream octets))
              (setf upload-size (length octets))))
           (t
            ;; Chunked
            (setf upload content))))
        (t
         (let ((octets (content-octets content external-format-out)))
           (setf upload (flex:make-in-memory-input-stream octets))
           (setf upload-size (if (integerp content-length)
                                 content-length
                                 (length octets)))))))

    (let* ((request-url (build-request-uri uri parameters parameters-used-p
                                           external-format-out url-encoder))
           (headers
             (remove
              nil
              (list*
               (when user-agent
                 (format nil "User-Agent: ~a" (drakma::user-agent-string user-agent)))
               (when basic-authorization
                 (format nil "Authorization: Basic ~a"
                         (cl-base64:string-to-base64-string
                          (format nil "~a:~a"
                                  (first basic-authorization)
                                  (second basic-authorization)))))
               (when accept
                 (format nil "Accept: ~a" accept))
               (when range
                 (format nil "Range: bytes=~a-~a" (first range) (second range)))
               (when (and content content-type)
                 (format nil "Content-Type: ~a" content-type))
               (when (and upload (not upload-size))
                 "Transfer-Encoding: chunked")
               ;; Don't wait for a 100 Continue before sending the body
               "Expect:"
               (when (and decode-content
                          (not (header-present-p "Accept-Encoding" additional-headers)))
                 "Accept-Encoding:")
               (loop for (name . value) in additional-headers
                     collect (format nil "~a: ~a"
                                     (header-name name)
                                     (header-value-string value))))))
           (handle (checkout-handle))
           (transfer (make-instance 'transfer
                                    :handle handle
                                    :url request-url
                                    :follow-redirects-p (and redirect t)
                                    :redirects-left (when (integerp redirect)
                                                      redirect)
                                    :upload upload
                                    :close-upload-p close-upload-p
                                    :connection-timeout connection-timeout
                                    ;; Upload progress resets the idle
                                    ;; clock too, so WRITE-TIMEOUT isn't
                                    ;; needed separately.
                                    :idle-timeout read-timeout))
           (returning-stream-p nil))
      (unwind-protect
           (progn
             (configure-handle transfer
                               :easy (easy handle)
                               :url request-url
                               :method method
                               :upload upload
                               :upload-size upload-size
                               :headers headers
                               :redirect redirect
                               :auto-referer auto-referer
                               :proxy proxy
                               :verify verify
                               :decode-content decode-content
                               :connection-timeout connection-timeout)
             (check-multi-code (curl-multi-add-handle (multi handle) (easy handle)))
             (setf (attachedp transfer) t)
             (cond
               (want-stream
                (drive transfer (lambda () (headers-done-p transfer))))
               (t
                (drive transfer (lambda () nil))))
             (unless (status transfer)
               (error 'curl-error
                      :url request-url
                      :message "Did not get an HTTP response"))
             (let* ((status (status transfer))
                    (headers (headers-alist transfer))
                    (external-format-body
                      (and (not force-binary)
                           (funcall drakma:*body-format-function*
                                    headers external-format-in)))
                    (uri (effective-uri transfer)))
               (cond
                 (want-stream
                  (let ((stream (make-instance 'curl-input-stream :transfer transfer)))
                    (setf returning-stream-p t)
                    (values
                     (apply #'flex:make-flexi-stream
                            stream
                            :external-format (or external-format-body
                                                 drakma::+latin-1+)
                            (when force-binary
                              (list :element-type 'flex:octet)))
                     status
                     headers
                     uri
                     stream
                     nil
                     (reason transfer))))
                 (t
                  (values
                   (cond
                     ((or (eql method :head)
                          (eql status 204))
                      nil)
                     ((and (equal "0" (a:assoc-value headers :content-length))
                           (not (a:assoc-value headers :transfer-encoding)))
                      ;; What Drakma does
                      nil)
                     (external-format-body
                      (flex:octets-to-string (all-body-octets transfer)
                                             :external-format external-format-body))
                     (t
                      (all-body-octets transfer)))
                   status
                   headers
                   uri
                   nil
                   nil
                   (reason transfer))))))
        (unless returning-stream-p
          (release-transfer transfer))))))

(defmethod low-level-request ((engine curl-engine) url &rest args &key &allow-other-keys)
  (cond
    ((and (libcurl-available-p)
          (supported-request-p url args))
     (apply #'curl-request url args))
    (t
     (call-next-method))))

;;; For checking our constants against the libcurl we loaded

(fli:define-c-struct curl-easyoption
  (name (:pointer :char))
  (id :int)
  (type :int)
  (flags :unsigned-int))

(fli:define-foreign-function (%curl-easy-option-by-name "curl_easy_option_by_name" :source)
    ((name :pointer))
  :result-type (:pointer (:struct curl-easyoption))
  :module :libcurl)

(defun option-id-by-name (name)
  "The CURLoption for e.g. \"URL\", or NIL if libcurl doesn't know it."
  (fli:with-foreign-string (ptr elements bytes) name
    (declare (ignore elements bytes))
    (let ((option (%curl-easy-option-by-name ptr)))
      (unless (fli:null-pointer-p option)
        (fli:foreign-slot-value option 'id)))))
