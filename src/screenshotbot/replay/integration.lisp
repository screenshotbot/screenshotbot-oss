;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/integration
  (:use #:cl)
  (:nicknames :screenshotbot/pro/replay/integration)
  (:import-from #:screenshotbot/replay/frontend
                #:width
                #:dimensions
                #:browser-type
                #:*default-browser-configs*
                #:screenshot-file-key
                #:screenshot-title
                #:screenshots
                #:job)
  (:import-from #:screenshotbot/sdk/sdk
                #:make-directory-run)
  (:import-from #:screenshotbot/sdk/git
                #:null-repo)
  (:import-from #:screenshotbot/sdk/flags
                #:*hostname*
                #:*api-secret*
                #:*api-key*)
  (:import-from #:screenshotbot/sdk/bundle
                #:local-image
                #:list-images)
  (:import-from #:screenshotbot/sdk/git
                #:make-instance)
  (:import-from #:util
                #:or-setf)
  (:import-from #:screenshotbot/replay/sitemap
                #:parse-sitemap)
  (:import-from #:screenshotbot/replay/core
                #:snapshot-request-sdk-flags
                #:asset-file-name
                #:context
                #:write-replay-log
                #:uuid
                #:root-asset
                #:asset-file
                #:load-url-into
                #:snapshot)
  (:import-from #:screenshotbot/replay/replay-acceptor
                #:with-hosted-snapshot
                #:call-with-hosted-snapshot)
  (:import-from #:screenshotbot/webdriver/impl
                #:with-webdriver
                #:call-with-webdriver)
  (:import-from #:screenshotbot/webdriver/screenshot
                #:full-page-screenshot)
  (:import-from #:webdriver-client
                #:window-resize)
  (:import-from #:auto-restart
                #:with-auto-restart)
  (:import-from #:screenshotbot/model/api-key
                #:api-key-secret
                #:make-transient-key)
  (:import-from #:screenshotbot/api-key-api
                #:api-key-secret-key
                #:api-key-key)
  (:import-from #:screenshotbot/replay/services
                #:with-selenium-server
                #:squid-proxy
                #:selenium-port
                #:selenium-host
                #:selenium-server
                #:selenium-server-url)
  (:import-from #:screenshotbot/replay/run-builder
                #:record-screenshot
                #:all-screenshots)
  (:import-from #:util/threading
                #:with-extras
                #:safe-interrupt-checkpoint)
  (:import-from #:screenshotbot/replay/proxy
                #:ensure-proxy)
  (:import-from #:util/object-id
                #:oid-array)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/sdk/common-flags
                #:*sdk-flags*)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:util/lists
                #:with-batches)
  (:local-nicknames (#:a #:alexandria)
                    (#:frontend #:screenshotbot/replay/frontend)
                    (#:integration #:screenshotbot/replay/integration)
                    (#:replay #:screenshotbot/replay/core)
                    (#:flags #:screenshotbot/sdk/flags))
  (:export
   #:sitemap
   #:run
   #:original-request))
(in-package :screenshotbot/replay/integration)

(with-auto-restart ()
 (defun get-local-addr (host port)
   (let ((socket (usocket:socket-connect host port)))
     (unwind-protect
          (progn
            #-lispworks
            (let ((local-name (usocket:get-local-name socket)))
              (str:join "." (loop for i across local-name
                                  collect (format nil "~d" i))))
            #+lispworks
            (let ((local-name (comm:get-socket-address (usocket:socket socket))))
              (comm:ip-address-string local-name)))
       (usocket:socket-close socket)))))

#+nil
(defun get-local-name (service)
  "Not currently in use. Mostly because we're hardcoding names in the
squid config, so if we do this we'd have to dynamically update squid
too."
  (typecase service
    (local-client
     "localhost")
    #+lispworks
    (t
     (get-local-addr (host service) (port service)))))

(defclass run ()
  ((company :initarg :company
            :reader company)
   (user :initarg :user
         :reader user)
   (sitemap :initarg :sitemap
            :initform nil
            :reader sitemap
            :documentation "URL to a sitemap. If Provided the list of URLs are picked from the sitemap")
   (exclusions :initarg :exclusions
               :initform nil
               :reader exclusions
               :documentation "List of regular expressions as string")
   (sampling :initarg :sampling
             :initform 1
             :reader sampling
             :documentation "Sampling rate from the list of URLs. Works when providing sitemap too.")
   (channel :initarg :channel
            :reader channel)
   (max-width :initarg :max-width
              :reader max-width
              :initform 640)
   (host :initarg :host
         :initform "https://api.screenshotbot.io"
         :reader host)
   (browser-configs :initarg :browser-configs
                    :initform *default-browser-configs*
                    :reader browser-configs)

   (request :initarg :request
            :initform nil
            :reader original-request)
   (urls :initarg :urls
         :initform nil
         :accessor %urls)
   (custom-css :initarg :custom-css
               :initform nil
               :accessor custom-css)
   (sleep :initarg :sleep
          :initform 0.5
          :reader sleep-time)))

(define-condition config-error (simple-error)
  ())

(defmethod initialize-instance :after ((self run) &key urls sitemap &allow-other-keys)
  (when (and urls sitemap)
    (error "Can't provide both urls and sitemap")))

(defmethod remove-base-url ((url string))
  (let ((uri (quri:uri url)))
    (quri:render-uri
     (quri:copy-uri uri
                    :scheme nil
                    :host nil))))

(defmethod urls ((run run))
  (or-setf
   (%urls run)
   (let ((exclusions (mapcar #'cl-ppcre:create-scanner
                             (exclusions run))))
    (and (sitemap run)
         (loop for url in (parse-sitemap (sitemap run))
               unless (loop for exclusion in exclusions
                            if (cl-ppcre:scan exclusion url)
                              return t)
               collect
               (cons
                (remove-base-url url)
                url))))))

(defmethod sampled-urls ((run run))
  "Determistically sample the list of URLS from run. Any
screenshotting operation should use this method instead of directly
accessing the urls or sitemap slot."
  (let ((urls (urls run))
        (sampling (* 256 (sampling run))))
    (loop for (name . url) in urls
          for hash = (elt (md5:md5sum-string url) 0)
          if (<= hash sampling)
            collect (cons name url))))

(def-easy-macro with-sdk-flags (&key flags &fn fn)
  (loop for (key . value) in flags
        for sym = (gethash key *sdk-flags*)
        collect sym into symbols
        collect value into values
        finally (progv symbols values
                  (funcall fn))))


(defun process-results (run results)
  ;; The SDK has an ugly API when used from a non-SDK world

  (restart-case
      (let* ((api-key (make-transient-key :user (user run)
                                          :company (company run)))
             (request (integration:original-request run))

             ;; There are two situations we could be here: we could be
             ;; here from the static website code from the SDK, or we
             ;; could be here from the web based replay jobs. We need
             ;; to set the flags manually for the web-based replay
             ;; jobs, but for the static website code, the
             ;; `with-sdk-flags` will propagate the flags.
             (flags:*api-key* (api-key-key api-key))
             (flags:*api-secret* (api-key-secret-key api-key))
             (flags:*hostname* (host run))
             (flags:*pull-request* (?. replay:pull-request request))
             (flags:*main-branch* (?. replay:main-branch request))
             (flags:*repo-url* (?. replay:repo-url request)))
        (with-sdk-flags (:flags (?. snapshot-request-sdk-flags request))
          (make-directory-run
           results
           :repo (make-instance 'null-repo)
           :branch "master"
           :commit (when request (replay:commit request))
           :merge-base (when request (replay:merge-base request))
           :branch-hash (when request (replay:branch-hash request))
           :github-repo (when request (replay:repo-url request))
           :periodic-job-p (or (not request) (str:emptyp (replay:commit request)))
           :is-trunk t
           :channel (channel run))))
    (retry-process-results ()
      (process-results run results))))

(defun run-replay-on-urls (&key (snapshot (error "provide snapshot"))
                                (replay-proxy (error "provide replay proxy"))
                             (urls (error "provide urls"))
                             (logger (lambda (url actual-url) (declare (ignore url actual-url))))
                             (hosted-url (error "provide hosted-url"))
                             (driver (error "provide driver"))
                             (config (error "provide config"))
                             (run (error "provide run"))
                             (tmpdir (error "provide tmpdir"))
                             (results (error "provide results")))
  (declare (ignore tmpdir)) ;; we used to use this to store images, no longer.
  (loop for (title . url) in urls
        for i from 0 do
          (restart-case
              (progn
                (safe-interrupt-checkpoint)
                (loop for root-asset in (replay:assets snapshot)
                      until (string= (replay:url root-asset) url)
                      finally
                         (let ((actual-url (quri:render-uri
                                            (quri:merge-uris
                                             (quri:uri
                                              (format nil "/company/~a/assets/~a"
                                                      (encrypt:encrypt-mongoid
                                                       (oid-array (company run)))
                                                      (asset-file-name root-asset)))
                                             (quri:uri
                                              hosted-url)))))

                           (funcall logger url actual-url)

                           (a:when-let (dimension (frontend:dimensions config))
                             (window-resize :width (frontend:width dimension)
                                            :height (frontend:height dimension)))
                           (setf (webdriver-client:url)
                                 actual-url)))

                ;; a temporary screenshot, I think this
                ;; will prime the browser to start loading
                ;; any assets that might be missing
                ;;(full-page-screenshot driver nil)


                ;; TODO: This sleep-time used to be part of
                ;; wait-for-zero-requests. Currently, because of
                ;; aggressive proxy caching, there's no way of waiting for
                ;; zero requests. In the previous logic we would *at
                ;; least* sleep for this much time, but might be more
                ;; while requests are pending. We might be able to reduce
                ;; this in the future, but it's not the bottleneck at time
                ;; of writing.
                (sleep (sleep-time run))

                (process-full-page-screenshot
                 driver
                 replay-proxy
                 :results results
                 :title (format nil "~a--~a"
                                title (frontend:browser-config-name config))))
            (ignore-this-url ()
              nil))))

(defmethod fetch-full-page-screenshot-handle (driver proxy)
  "Creates a full-page-screenshot, and returns two values: the handle
  for the screenshot and the md5sum of the screenshot"
  (let* ((url (format nil "~a/full-page-screenshot"
                      proxy))
         (resp (uiop:slurp-input-stream
                'string
                (util/request:http-request
                 url
                 :method :post
                 :parameters `(("session" . ,(webdriver-client::session-id webdriver-client::*session*))
                               ("browser" . ,(string-downcase (type-of driver)))
                               ("uri" . ,webdriver-client::*uri*))
                 :want-stream t
                 :connection-timeout 15
                 ;; this request can be slow! Also give it some time
                 ;; in case we're in the debugger.
                 :read-timeout 1200)))
         (json-response (json:decode-json-from-string resp))
         (oid (a:assoc-value json-response :oid))
         (md5 (a:assoc-value json-response :md-5)))
    (values oid md5)))

(defmethod write-full-page-screenshot-from-handle (driver proxy oid dest)
  (with-open-stream (stream (util/request:http-request
                             (format nil "~a/download?oid=~a" proxy oid)
                             :force-binary t
                             :want-stream t))
    (with-open-file (dest dest :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8)
                               :if-does-not-exist :create)
      (uiop:copy-stream-to-stream
       stream dest
       :element-type '(unsigned-byte 8)))))

(auto-restart:with-auto-restart ()
 (defun process-full-page-screenshot (driver
                                      proxy
                                      &key title
                                        results)
   (multiple-value-bind (oid md5)
       (fetch-full-page-screenshot-handle driver proxy)
     (assert oid)
     (assert md5)
     (record-screenshot
      results
      :md5 md5
      :fetch (lambda (dest)
               (write-full-page-screenshot-from-handle
                driver
                proxy
                oid
                dest))
      :title title))))

(with-auto-restart ()
  (defun replay-job-from-snapshot (&key (snapshot (error "must provide snapshot"))
                                     urls run tmpdir)
   (let* ((results (make-instance 'all-screenshots
                                   :company (company run)))
          (url-count  (length urls)))
     (prog1 results
       (let ((configs (browser-configs run)))
         (assert configs)
         (dolist (config configs)
           (with-selenium-server (selenium-server :type (browser-type config))
             (with-hosted-snapshot (hosted-url (company run) snapshot
                                    :hostname (get-local-addr
                                               (selenium-host selenium-server)
                                               (selenium-port selenium-server)))
               (let ((webdriver-client::*uri*
                       (selenium-server-url selenium-server)))
                 (write-replay-log "Waiting for Selenium worker of type ~a" (browser-type config))
                 (with-batches (urls urls :index idx)
                   (with-webdriver (driver
                                    :proxy (squid-proxy selenium-server)
                                    :browser (frontend:browser-type config)
                                    :dimensions (when (frontend:dimensions config)
                                                  (cons
                                                   (frontend:width (dimensions config))
                                                   (frontend:height (dimensions config))))
                                    :mobile-emulation (frontend:mobile-emulation config))
                     ;; We have our browser and our hosted snapshots, let's go through this
                     (write-replay-log "Selenium worker is ready")
                     (run-replay-on-urls
                      :snapshot snapshot
                      :replay-proxy (ensure-proxy selenium-server)
                      :urls urls
                      :hosted-url hosted-url
                      :driver driver
                      :logger (lambda (url actual-url)
                                (write-replay-log "[~a/~a] Running url: ~a / ~a"
                                                  (incf idx)
                                                  url-count  url actual-url))
                      :config config
                      :run run
                      :tmpdir tmpdir
                      :results results))))))))))))


(defun best-image-type (config)
  (cond
    ((string-equal "firefox" (browser-type config))
     "png")
    (t
     "png")))

(with-auto-restart ()
  (defun schedule-replay-job (run)
    (with-extras (("run" run)
                  ("company" (company run)))
     (tmpdir:with-tmpdir (tmpdir)
       (handler-bind ((dex:http-request-failed
                        (lambda (e)
                          (write-replay-log "HTTP request failed: ~a~%" (type-of e))))
                      (cl+ssl::hostname-verification-error
                        (lambda (e)
                          (write-replay-log "SSL error: ~S~%" e))))
         (let* ((urls (sampled-urls run))
                (snapshot (make-instance 'snapshot :tmpdir tmpdir))
                (context (make-instance 'context
                                        :custom-css (custom-css run)))
                (count (length urls)))
           (loop for (nil . url) in urls
                 for i from 1
                 do
                    (restart-case
                        (progn
                          (log:info "Loading ~a/~a" i count)
                          (load-url-into context snapshot url tmpdir))
                      (ignore-this-url ()
                        (values))))
           (let ((results (replay-job-from-snapshot
                           :snapshot snapshot
                           :urls urls
                           :tmpdir tmpdir
                           :run run)))
             (process-results run results))))))))

#+nil
(schedule-replay-job (make-instance 'run
                                    :channel "test-channel"
                                    :urls (list "https://staging.screenshotbot.io")
                                    :api-key "FR7L47QK3YHMZ3Z8TEAX"
                                    :api-secret "F70tzxRVRf2VgkCTZH0j7nb9HABchakx5LEbL9lm"))
