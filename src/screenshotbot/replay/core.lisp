;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/core
  (:use #:cl)
  (:import-from #:json-mop
                #:json-serializable
                #:json-serializable-class)
  (:import-from #:auto-restart
                #:with-auto-restart)
  (:import-from #:util/misc
                #:or-setf)
  (:import-from #:util/threading
                #:safe-interrupt-checkpoint)
  (:import-from #:util/digests
                #:sha256-file)
  (:import-from #:util/lru-cache
                #:with-cache-file)
  (:import-from #:util/http-cache
                #:parse-max-age)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:util/request
                #:http-request-impl
                #:engine)
  (:import-from #:util/engines
                #:handle-misbehaving-engine)
  (:import-from #:util/json-mop
                #:ext-json-serializable-class)
  (:import-from #:screenshotbot/replay/browser-config
                #:browser-config)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:rewrite-css-urls
   #:*replay-logs*
   #:tmpdir
   #:asset-file
   #:asset-file
   #:asset-response-headers
   #:asset-status
   #:load-url
   #:snapshot
   #:load-url-into
   #:snapshot-asset-file
   #:uuid
   #:assets
   #:url
   #:http-header-name
   #:http-header-value
   #:snapshot-request
   #:browser-configs
   #:channel-name
   #:root-assets
   #:pull-request
   #:main-branch
   #:repo-url
   #:merge-base
   #:commit
   #:branch-hash
   #:request-counter
   #:call-with-request-counter
   #:write-replay-log
   #:context
   #:asset-file-name
   #:parse-max-age))
(in-package :screenshotbot/replay/core)

(defconstant +empty-headers+ nil)

(defvar *replay-logs* *terminal-io*)

(defvar *timeout* 15)

(defvar *stack* nil)

(defclass snapshot-request ()
  ((snapshot :initarg :snapshot
             :json-key "snapshot"
             :json-type snapshot
             :reader snapshot)
   (browser-configs :initarg :browser-configs
                    :json-key "browserConfigs"
                    :json-type (:list browser-config)
                    :reader browser-configs)
   (channel-name :initarg :channel-name
                 :json-key "channel"
                 :json-type :string
                 :reader channel-name)
   (pull-request :initarg :pull-request
                 :json-key "pullRequestUrl"
                 :json-type (or null :string)
                 :initform nil
                 :reader pull-request)
   (main-branch :initarg :main-branch
                :json-key "mainBranch"
                :json-type (or null :string)
                :initform nil
                :reader main-branch)
   (repo-url :initarg :repo-url
             :json-key "repoUrl"
             :json-type (or null :string)
             :initform nil
             :reader repo-url)
   (branch-hash :initarg :branch-hash
                :json-key "branchHash"
                :json-type (or null :string)
                :initform nil
                :reader branch-hash)
   (commit :initarg :commit
           :json-key "commit"
           :json-type (or null :string)
           :initform nil
           :reader commit)
   (sdk-flags :initform nil
              :json-key "sdkFlags"
              :json-type (or null :string)
              :initarg :sdk-flags
              :reader snapshot-request-sdk-flags)
   (merge-base :initarg :merge-base
               :json-key "mergeBase"
               :json-type (or null :string)
               :initform nil
               :reader merge-base))
  (:metaclass ext-json-serializable-class))

(defclass context ()
  ((seen :initform (make-hash-table :test #'equal)
         :reader context-seen)
   (custom-css :initarg :custom-css
               :initform nil
               :reader custom-css))
  (:documentation "A crawler context. Keeps track of all DFS nodes
  we're looking at, and also allows us to customize the node
  processing for specific websites"))

(defclass http-header ()
  ((name :initarg :name
         :json-type :string
         :json-key "name"
         :reader http-header-name)
   (value :initarg :value
          :json-type :string
          :json-key "value"
          :reader http-header-value))
  (:metaclass ext-json-serializable-class))

(defmethod initialize-instance :after ((self http-header) &key name)
  (when (symbolp name)
    (setf (slot-value self 'name) (string-downcase (string name)))))

(defclass asset ()
  ((file :initarg :file
         :json-type :string
         :json-key "file"
         :reader asset-file)
   (url :initarg :url
        :json-type :string
        :json-key "url"
        :reader url)
   (status :initarg :status
           :json-type :number
           :json-key "status"
           :reader asset-status)
   (stylesheetp :initarg :stylesheetp
                :json-type :bool
                :json-key "isStylesheet"
                :initform nil
                :reader stylesheetp)
   (response-headers :initarg :response-headers
                     :json-type (:list http-header)
                     :json-key "responseHeaders"
                     :reader asset-response-headers))
  (:metaclass ext-json-serializable-class))

(defmethod asset-file-name ((asset asset))
  (car (last (str:split "/" (asset-file asset)))))

(defmethod print-object ((asset asset) out)
  (format out "#<ASSET ~a ~a>" (asset-file asset) (url asset)))

(defmethod initialize-instance :after ((self asset) &key &allow-other-keys)
  )

(defmethod class-persistent-slots ((self asset))
  '(file status stylesheetp response-headers))

(defclass snapshot ()
  ((assets :initform nil
           :initarg :assets
           :json-type (:list asset)
           :json-key "assets"
           :accessor assets)
   (uuid :initform (format nil "~a" (uuid:make-v4-uuid))
         :json-type :string
         :json-key "uuid"
         :reader uuid)
   (tmpdir :initarg :tmpdir
           :accessor tmpdir)
   (root-files :initarg :root-files
               :initform nil
               :documentation "DEPRECATED: See T621, as we're migrating this away to root-urls")
   (root-urls :accessor root-urls
              :json-type (:list :string)
              :json-key "rootUrls"
              :initarg :root-urls
              :initform nil
              :documentation "The URLs that were used for the root")
   (request-counter :accessor request-counter
                    :initform 0
                    :documentation "When the snapshot is being served,
                    this keeps track of the total number of current
                    requests. This allows us to wait until all assets
                    are served before taking screenshots."))
  (:metaclass ext-json-serializable-class))

(defvar *request-counter-lock* (bt:make-lock "request-counter-lock"))

(defclass request-engine (handle-misbehaving-engine
                          engine)
  ())

(defvar *request-engine* (make-instance 'request-engine))

(defmethod call-with-request-counter ((snapshot snapshot) fn)
  (unwind-protect
       (progn
         (bt:with-lock-held (*request-counter-lock*)
           (incf (request-counter snapshot)))
         (funcall fn))
    (bt:with-lock-held (*request-counter-lock*)
      (decf (request-counter snapshot)))))

(defun not-null! (expr)
  (assert expr)
  expr)

(defun root-assets (snapshot)
  (let ((url-to-asset-map
          (reduce
           (lambda (map asset)
             (declare (type asset asset)
                      (type string url))
             (fset:with map (url asset) asset))
           (assets snapshot)
           :initial-value (fset:empty-map))))
    (reverse
     (loop for url in (root-urls snapshot)
           collect
           (not-null! (fset:lookup url-to-asset-map url))))))

(defmethod process-node (context node snapshot url)
  (values))

(defun fix-asset-headers (headers)
  "Fix some bad headers"
  (loop for (k . v) in headers
        if (string-equal "Access-Control-Allow-Origin" k)
          collect (cons k "*")
        else
          collect (cons k v)))

(define-condition recursive-asset-error (error)
  ((url :initarg :url)))

(defmethod call-with-fetch-asset ((context context)
                                  fn type tmpdir &key url
                                                   (snapshot (error "must provide snapshot")))
  (when (member url *stack* :test #'equal)
    (error 'recursive-asset-error
            :url url))
  (let ((*stack* (cons url *stack*)))
   (multiple-value-bind (remote-stream status response-headers)
       (funcall fn)
     ;; dex:get should handle redirects, but just in case:
     (with-open-stream (input remote-stream)
       (assert (not (member status `(301 302))))
       (fix-asset-headers response-headers)

       (uiop:with-temporary-file (:pathname p :stream out :directory tmpdir :keep t
                                  :element-type '(unsigned-byte 8)
                                  :direction :io)
         (uiop:copy-stream-to-stream input out :element-type '(unsigned-byte 8))
         (file-position out 0)
         (finish-output out)
         (close out)

         (write-asset p type
                      :tmpdir tmpdir
                      :url url
                      :snapshot snapshot
                      :response-headers response-headers
                      :status status))))))


(defun write-replay-log (message &rest args)
  (unless (eql *terminal-io* *replay-logs*)
    (local-time:format-timestring
     *replay-logs*
     (local-time:now) :format `("[" (:hour 2) ":" (:min 2) ":" (:sec 2) "] "))
    (apply #'format *replay-logs* message args)
    (format *replay-logs* "~%")
    (finish-output *replay-logs*)))

(defun hash-file (file)
  (sha256-file file))

(defun write-asset (p type &key tmpdir
                             url
                             (snapshot (error "must provide snapshot"))
                             response-headers
                             stylesheetp
                             status)
  (let ((hash (ironclad:byte-array-to-hex-string (hash-file p))))
    (uiop:rename-file-overwriting-target
     p (make-pathname
        :name hash
        :type type
        :defaults tmpdir))
    (make-instance
     'asset
     :file (format nil
                   "/snapshot/~a/assets/~a"
                   (uuid snapshot)
                   (make-pathname :name hash :type type
                                  :directory `(:relative)))
     :url (cond
            ((stringp url)
             url)
            (t
             (quri:render-uri url)))
     :response-headers (loop for (k . v) in response-headers
                             collect
                             (make-instance 'http-header
                                            :name k
                                            :value v))
     :stylesheetp stylesheetp
     :status status)))

(defun fix-malformed-url (url)
  (let* ((url (quri:render-uri (quri:uri url)))
         (url (str:replace-all " " "%20" url))
         (url (str:replace-all "[" "%5B" url))
         (url (str:replace-all "]" "%5D" url)))
    url))

(defun remove-unwanted-headers (response-headers)
  (remove-if
   (lambda (key)
     (or
      (member key '(:content-security-policy
                    ;; The next two are just not
                    ;; needed, so we'll let them be
                    ;; GC-ed earlier
                    :connection
                    :alt-svc))
      (char-equal #\x (elt (string key) 0))))
   response-headers
   :key #'car))

(defmethod http-request-impl ((engine request-engine)
                              url &rest args)
  (let* ((url (fix-malformed-url url))
         (url (quri:uri url))
         (scheme (quri:uri-scheme url)))
    (cond
      ((equal "file" scheme)
       (error "file:// urls are not supported"))
      ((member scheme (list "chrome-extension") :test #'string-equal)
       ;; respond with an empty file
       (values
        (make-string-input-stream "")
        200
        +empty-headers+))
      ((or (equal "https" scheme)
           (equal "http" scheme))

       (log:info "Fetching: ~a" url)
       (write-replay-log "Fetching: ~a" url)

       (multiple-value-bind (remote-stream status response-headers)
           (call-next-method)
         (let ((response-headers
                 (remove-unwanted-headers response-headers)))
           (push `(:x-original-url . ,(quri:render-uri url)) response-headers)
           (values remote-stream status response-headers))))
      (t
       (error "unsupported scheme: ~a" scheme)))))

(defun http-get-without-cache (url &key (force-binary t)
                                     (force-string nil))
  (declare (ignore force-string))

  (util/request:http-request (format nil "~a" url)
                             :want-stream t :force-binary force-binary
                             :read-timeout *timeout*
                             :accept "image/webp,*/*"
                             :connection-timeout *timeout*
                             :engine *request-engine*))

(defvar *cache* nil)

(defun http-cache-dir ()
  (let ((object-store-sym (find-symbol "*OBJECT-STORE*" "UTIL")))
    (unless object-store-sym
      (error "HTTP-CACHE-DIR not supported in SDK, most likely a bug: please reach out to arnold@screenshotbot.io"))
    (let ((store-dir (symbol-value object-store-sym)))
      (let ((dir (path:catdir
                  store-dir
                  "cache/replay-http-cache/")))
        (ensure-directories-exist dir)
        (log:debug "Using cache dir for http: ~A" dir)
        dir))))

(defun %lru-cache ()
  (util/misc:or-setf
   *cache*
   (make-instance 'util/lru-cache:lru-cache
                  :dir (http-cache-dir)
                  :max-size "2GB")))

(defclass remote-response ()
  ((status :initarg :status
           :reader remote-response-status)
   (request-time :initarg :request-time
                 :initform 0
                 :reader request-time)
   (headers :initarg :headers
            :reader remote-response-headers)))

(defmethod max-age ((self remote-response))
  (let ((cache-control (assoc-value (remote-response-headers self) :cache-control)))
    (log:debug "Got cache-control: ~a" cache-control)
    (cond
      (cache-control
       (parse-max-age cache-control))
      (t
       -1))))

(defun remove-quotes (str)
  (cond
    ((member (elt str 0)
             (list #\' #\"))
     (str:substring 1 -1 str))
    (t
     str)))

(defmethod guess-external-format ((info remote-response)
                                  file-name)
  (or
   (a:when-let ((content-type (assoc-value (remote-response-headers info) :content-type)))
     (loop for part in (str:split ";" content-type)
           do
              (destructuring-bind (key &optional value) (str:split "=" (str:trim part))
                (when (string-equal (str:trim key) "charset")
                  (let* ((charset (remove-quotes (str:downcase (str:trim value)))))
                    (return
                      (cond
                        ((or
                          (string= charset "utf-8")
                          (string= charset "utf8"))
                         :utf-8)
                        ((or
                          (string= charset "iso-8859-1")
                          (string= charset "latin-1"))
                         :latin-1)
                        (t
                         (error "unspported charset ~a" charset)))))))))
   (ignore-errors
    (guess-content-type-from-file file-name))
   :latin-1))

(defun guess-content-type-from-file (file)
  (with-open-file (stream file :direction :input)
    (let ((html (plump:parse stream)))
      (labels ((charset (tag)
                 (plump:attribute tag "charset"))
               (find-meta (tag)
                 (cond
                   ((and
                     (typep tag 'plump:element)
                     (string-equal "meta" (plump:tag-name tag))
                     (charset tag))
                    (cond
                      ((string-equal "utf-8" (charset tag))
                       :utf-8)))
                   ((typep tag 'plump:nesting-node)
                    (loop for child across (plump:children tag)
                          for charset = (find-meta child)
                          if charset
                            return charset)))))
        (find-meta html)))))

(defun http-get (url &key (force-binary t)
                     (force-string nil)
                       (cache t))
  (let* ((url (quri:uri url))
         (cache-key (format
                     nil "~a-~a-v6"
                     (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 (flexi-streams:string-to-octets  (quri:render-uri url))))
                     force-binary)))
    (with-cache-file (output (%lru-cache) (make-pathname :name cache-key :type "data"))
      (with-cache-file (info (%lru-cache) (make-pathname :name cache-key :type "info-v3"))
       (flet ((read-cached ()
                (let ((info (cl-store:restore info)))
                  (log:debug "Opening from cache: ~a" output)
                  (values
                   (open output :element-type (cond
                                                (force-binary
                                                 '(unsigned-byte 8))
                                                (force-string
                                                 'character)
                                                (t
                                                 'character))
                                :external-format (or
                                                  (unless force-binary
                                                    (guess-external-format info output))
                                                  :default))
                   (remote-response-status info)
                   (remote-response-headers info))))
              (good-cache-p (file)
                (uiop:file-exists-p file)))
         (cond
           ((and cache (every #'good-cache-p (list info output))
                 (let ((info (cl-store:restore info)))
                   (and
                    (slot-boundp info 'request-time) #| Compatibility with old cache|#
                    (max-age info)
                    (< (get-universal-time)
                       (+ (max-age info) (request-time info))))))
            (write-replay-log "Using cached asset for ~a" url)
            (read-cached))
           (t
            ;; we're not cached yet
            (multiple-value-bind (stream %status %headers)
                (handler-case
                    (http-get-without-cache url :force-binary t)
                  (puri:uri-parse-error (e)
                    (write-replay-log "Invalid URL: ~a, will treat as 500" url)
                    (values
                     (make-string-input-stream "")
                     500
                     +empty-headers+)))
              (with-open-file (output output :element-type '(unsigned-byte 8)
                                             :if-exists :supersede
                                             :direction :output)
                (uiop:copy-stream-to-stream stream output :element-type '(unsigned-byte 8)))
              (cl-store:store (make-instance 'remote-response
                                              :status %status
                                              :request-time (get-universal-time)
                                              :headers %headers)
                              info))
            (log:debug "Caching for the first time ~a" output)
            (read-cached))))))))

(with-auto-restart ()
 (defmethod fetch-asset ((context context) url tmpdir (snapshot snapshot))
   "Fetches the url into a file <hash>.<file-type> in the tmpdir."
   (let ((pathname (ignore-errors (quri:uri-file-pathname url))))
     (call-with-fetch-asset
      context
      (lambda ()
        (log:debug "v1 of fetch-asset")
        (http-get url))
      (cond
        (pathname
         (pathname-type pathname))
        (t
         nil))
      tmpdir
      :url url
      :snapshot snapshot))))

(defparameter *regexs*
  (uiop:read-file-lines
   (asdf:system-relative-pathname :screenshotbot "replay/replay-regex.txt")))

(defun should-rewrite-url-p (url)
  (let ((untouched-schemes (list "data:" "moz-extension:")))
    (loop for scheme in untouched-schemes
            always
            (not (str:starts-with-p scheme url)))))

(defun rewrite-css-urls (css fn)
  (destructuring-bind (property-matcher url-matcher) *regexs*
    (declare (ignore property-matcher))
    (let ((url-scanner (cl-ppcre:create-scanner url-matcher)))
      (cl-ppcre:regex-replace-all
       url-scanner
       css
       (lambda (match start end match-start match-end reg-starts reg-ends)
         (declare (ignore start end match-start match-end))
         (let ((url (subseq match (elt reg-starts 0)
                            (elt reg-ends 0))))
           (cond
             ((not (should-rewrite-url-p url))
              ;; we never wan't to rewrite data urls
              url)
             (t
              (format nil "url(~a)" (funcall fn url))))))))))

(defmethod rewrite-css ((context context) (snapshot snapshot) url css)
  (flet ((rewrite-url (this-url)
           (let ((full-url (quri:merge-uris (quri:uri this-url) url)))
             (asset-file-name
              (push-asset context snapshot full-url nil)))))
    (let* ((css (rewrite-css-urls css #'rewrite-url)))
      css)))


(defmethod fetch-css-asset ((context context) (snapshot snapshot) url tmpdir)
  (log:debug "calling from fetch-css-asset")
  (multiple-value-bind (remote-stream status response-headers) (http-get url :force-binary nil)
    (with-open-stream (remote-stream remote-stream)
     (uiop:with-temporary-file (:stream out :pathname p :type "css"
                                :directory (tmpdir snapshot))
       (let* ((css (uiop:slurp-stream-string remote-stream))
              (css (rewrite-css context snapshot url css)))
         (write-string css out)
         (finish-output out))
       (close out) ;; required for windows
       (write-asset p "css"
                    :tmpdir (tmpdir snapshot)
                    :url url
                    :status status
                    :snapshot snapshot
                    :stylesheetp t
                    :response-headers response-headers)))))

(defmethod push-asset ((context context) (snapshot snapshot) url stylesheetp)
  (let ((rendered-uri (quri:render-uri url)))
   (let ((stylesheetp (or stylesheetp (str:ends-with-p ".css" rendered-uri))))
     (loop for asset in (assets snapshot)
           for this-url = (url asset)
           do (check-type this-url string))
     (loop for asset in (assets snapshot)
           if (string= (url asset) rendered-uri)
             do
                (return asset)
           finally
              (let ((asset (cond
                             (stylesheetp
                              (fetch-css-asset context snapshot url (tmpdir snapshot)))
                             (t
                              (fetch-asset context url (tmpdir snapshot) snapshot)))))
                (push asset (assets snapshot))
                (return asset))))))

(defun read-srcset (srcset)
  (flet ((read-spaces (stream)
           (loop while (let ((ch (peek-char nil stream nil)))
                         (and ch
                              (str:blankp ch)))
                     do (read-char stream nil))))
   (let ((stream (make-string-input-stream srcset)))
     (read-spaces stream)
     (loop while (peek-char nil stream nil)
           collect
           (cons
            (str:trim
             (with-output-to-string (url)
               (loop for ch = (read-char stream nil)
                     while (and ch
                                (not (str:blankp ch)))
                     do
                        (write-char ch url))
               (read-spaces stream)))
            (str:trim
             (with-output-to-string (width)
               (loop for ch = (read-char stream nil)
                     while (and ch
                                (not (eql #\, ch)))
                     do
                        (write-char ch width))
               (read-spaces stream))))))))

(defmethod process-node ((context context)
                         (node plump:element) snapshot root-url)
  (let ((name (plump:tag-name node)))
    (labels ((? (test-name)
               (string-equal name test-name))
             (safe-replace-attr (name fn)
               "Replace the attribute if it exists with the output from the function"
               (let ((val (plump:attribute node name)))
                 (when (and val
                            (should-rewrite-url-p val))
                   (let* ((ref-uri val)
                          (uri (quri:merge-uris
                                (quri:uri ref-uri)
                                (quri:uri root-url))))
                     (setf (plump:attribute node name) (funcall fn uri))))))
             (replace-attr (name &optional stylesheetp)
               (safe-replace-attr
                name
                (lambda (uri)
                  (let* ((asset (push-asset context snapshot uri stylesheetp))
                         (res (asset-file-name asset)))
                    res))))
             (parse-intrinsic (x)
               (parse-integer (str:replace-all "w" "" x)))
             (replace-srcset (name)
               (ignore-errors
                (let ((srcset (plump:attribute node name)))
                  (when srcset
                    (let* ((data (read-srcset srcset))
                           (smallest-width
                             (loop for (nil . width) in data
                                   minimizing (parse-intrinsic width)))
                           (max-width (max 500 smallest-width)))

                      (let ((final-attr
                              (str:join
                               ","
                               (loop for (url . width) in  data
                                     for uri = (quri:merge-uris (quri:uri url) (quri:uri root-url))
                                     if (<= (parse-intrinsic width) max-width)
                                       collect
                                       (let ((asset (push-asset context snapshot uri nil)))
                                         (str:join " "
                                                   (list
                                                    (asset-file-name asset)
                                                    width)))))))
                        (setf (plump:attribute node name)
                              final-attr))))))))
      (cond
       ((or (? "img")
            (? "source"))
        (replace-attr "src")
        (plump:remove-attribute node "srcset")
        ;;(replace-srcset "srcset")
        ;; webpack? Maybe?
        ;;(replace-attr "data-src")
        ;;(replace-srcset "data-srcset")
        (plump:remove-attribute node "decoding")
        (setf (plump:attribute node "loading") "eager")
        (plump:remove-attribute node "data-gatsby-image-ssr"))
       ((? "picture")
        (replace-srcset "srcset"))
       ((? "iframe")
        (setf (plump:attribute node "src")
              "/iframe-not-supported"))
       ((? "style")
        (loop for child across (plump:children node)
              do (setf (plump:text child)
                       (rewrite-css context
                                    snapshot
                                    root-url
                                    (plump:text child)))))
       ((? "video")
        ;; autoplay videos mess up screenshots
        (plump:remove-attribute node "autoplay"))
       ((? "link")
        (let ((rel (plump:attribute node "rel")))
         (cond
           ((string-equal "canonical" rel)
            ;; do nothing
            (values))
           ((string-equal "dns-prefetch" rel)
            (values))
           (t
            (replace-attr "href" (string-equal "stylesheet" rel))))))
       ((? "script")
        (replace-attr "src"))
       ((? "input")
        (plump:remove-attribute node "autofocus"))
       ((? "body")
        (let ((old-class (or (plump:attribute node "class") "")))
          (setf (plump:attribute node "class") (format nil "~a screenshotbot" old-class)))))))
  (call-next-method))


(defmethod process-node ((context context) (node plump:nesting-node) snapshot root-url)
  (loop for child across (plump:children node)
        do (process-node context child snapshot root-url))
  (call-next-method))

(defmethod process-node :before ((context context) node snapshot root-url)
  ;;(log:info "Looking at: ~S" node)
  )

(defmethod add-css ((context context) html)
  "Add the replay css overrides to the html"
  (a:when-let (head (ignore-errors (elt (lquery:$ html "head") 0)))
    (let ((link (plump:make-element
                        head "link")))
      (setf (plump:attribute link "href") "/css/replay.css")
      (setf (plump:attribute link "rel") "stylesheet"))
    (when (custom-css context)
      (let ((style (plump:make-element head "style")))
        (plump:make-text-node style (custom-css context))))))

(defun safe-plump-serialize (&rest args)
  (flet ((ignore-handler (e)
           (declare (ignore e))
           (invoke-restart 'cl:continue)))
    (handler-bind ((plump-dom:invalid-xml-character #'ignore-handler)
                   (plump-dom:discouraged-xml-character #'ignore-handler))
      (apply #'plump:serialize args))))

(with-auto-restart (:retries 0 :sleep 10)
  (defmethod load-url-into ((context context) snapshot url tmpdir
                            &rest args
                            &key
                              &allow-other-keys)
    (safe-interrupt-checkpoint)
    (with-open-stream (content (http-get url :force-string t
                                             :force-binary nil))

      (apply
       #'parse-after-fetching context
       snapshot
       url
       tmpdir
       :content content
       args))))

(with-auto-restart ()
  (defmethod parse-after-fetching ((context context)
                                   snapshot
                                   url
                                   tmpdir
                                   &key actual-url
                                     content)
    (file-position content 0) ;; in case of a restart
    (let* ((url (quri:uri url))
           (html (plump:parse content)))
      (process-node context html snapshot
                    ;; If we pass an actual-url, then we should use the
                    ;; actual URL to figure out where to fetch assets
                    ;; from
                    (or actual-url url))
      (add-css context html)

      #+nil
      (error "got html: ~a"
             (with-output-to-string (s)
               (plump:serialize html s)))
      (uiop:with-temporary-file (:direction :io :stream tmp :element-type '(unsigned-byte 8))
        (let ((root-asset (call-with-fetch-asset
                           context
                           (lambda ()
                             (log:debug "v2 of fetch asset")
                             (safe-plump-serialize html (flexi-streams:make-flexi-stream tmp :element-type 'character :external-format :utf-8))
                             (file-position tmp 0)
                             (let ((headers `((:content-type . "text/html; charset=UTF-8")
                                              (:cache-control . "no-cache"))))
                               (values tmp 200 headers)))
                           "html"
                           tmpdir
                           :url url
                           :snapshot snapshot)))
          (push
           (quri:render-uri url)
           (root-urls snapshot))
          (push root-asset
                (assets snapshot))))
      snapshot)))

(defmethod snapshot-asset-file ((snapshot snapshot)
                                (asset asset))
  (let* ((script-name (asset-file asset))
         (input-file (make-pathname
                      :name (pathname-name script-name)
                      :type (pathname-type script-name)
                      :defaults (tmpdir snapshot))))
    input-file))

(defun load-url (url tmpdir)
  (let ((snapshot (make-instance 'snapshot :tmpdir tmpdir)))
    (load-url-into (make-instance 'context) snapshot url tmpdir)))
