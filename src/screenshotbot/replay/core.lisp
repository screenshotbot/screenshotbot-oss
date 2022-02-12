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
   #:root-files
   #:url
   #:http-header-name
   #:http-header-value
   #:snapshot-request))
(in-package :screenshotbot/replay/core)


(defvar *replay-logs* *terminal-io*)

(defclass snapshot-request ()
  ((snapshot :initarg :snapshot
             :reader snapshot)
   (browser-configs :initarg :browser-configs
                    :reader browser-configs)))

(defclass http-header ()
  ((name :initarg :name
         :reader http-header-name
         :json-key "name")
   (value :initarg :value
          :reader http-header-value
          :json-key "value"))
  (:metaclass json-serializable-class))

(defclass asset ()
  ((file :initarg :file
         :reader asset-file
         :json-key "file"
         :json-type :string)
   (url :initarg :url
        :reader url
        :json-key "url"
        :json-type :string)
   (status :initarg :status
           :reader asset-status
           :json-key "assetStatus"
           :json-type :number)
   (stylesheetp :initarg :stylesheetp
                :initform nil
                :json-key "hasStylesheet"
                :reader stylesheetp
                :json-type :bool)
   (response-headers :initarg :response-headers
                     :reader asset-response-headers
                     :json-key "responseHeaders"
                     :json-type (:list http-header)))
  (:metaclass json-serializable-class))

(defmethod initialize-instance :after ((self asset) &key &allow-other-keys)
  )

(defmethod class-persistent-slots ((self asset))
  '(file status stylesheetp response-headers))

(defclass snapshot ()
  ((assets :initform nil
           :json-key "assets"
           :accessor assets
           :json-type (:list asset))
   (uuid :initform (format nil "~a" (uuid:make-v4-uuid))
         :reader uuid
         :json-key "uuid"
         :json-type :string)
   (tmpdir :initarg :tmpdir
           :accessor tmpdir)
   (root-files :accessor root-files
               :json-key "rootFiles"
               :initform nil
               :json-type (:list :string)))
  (:metaclass json-serializable-class))

(defmethod process-node (node snapshot url)
  (values))

(defun fix-asset-headers (headers)
  "Fix some bad headers"
  (loop for k being the hash-keys of headers
        if (string-equal "Access-Control-Allow-Origin" k)
          do (setf (gethash k headers) "*")
        if (string-equal "Cache-control" k)
          do (Setf (gethash k headers) "no-cache")))

(defun call-with-fetch-asset (fn type tmpdir &key url
                                               (snapshot (error "must provide snapshot")))
  (multiple-value-bind (remote-stream status response-headers)
      (funcall fn)
    ;; dex:get should handle redirects, but just in case:
    (assert (not (member status `(301 302))))
    (fix-asset-headers response-headers)

    (uiop:with-temporary-file (:pathname p :stream out :directory tmpdir :keep t
                               :element-type '(unsigned-byte 8)
                               :direction :io)
      (with-open-stream (input remote-stream)
        (uiop:copy-stream-to-stream input out :element-type '(unsigned-byte 8))
        (file-position out 0))
      (finish-output out)

      (write-asset p type
                   :tmpdir tmpdir
                   :url url
                   :snapshot snapshot
                   :response-headers response-headers
                   :status status))))

(defun hash-file (file)
  (ironclad:digest-file :sha256 file))

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
     :response-headers (loop for k being the hash-keys in response-headers
                                             using (hash-value v)
                             collect
                             (make-instance 'http-header
                                            :name k
                                            :value v))
     :stylesheetp stylesheetp
     :status status)))

(defun http-get-without-cache (url &key (force-binary t)
                                     (force-string nil))
  (let* ((url (quri:uri url))
         (scheme (quri:uri-scheme url)))
    (cond
      ((equal "file" scheme)
       (error "file:// urls are not supported"))
      ((or (equal "https" scheme)
           (equal "http" scheme))
       (multiple-value-bind (remote-stream status response-headers)
           (handler-bind ((dex:http-request-failed
                            (lambda (e)
                              (return-from
                               http-get-without-cache
                                (values
                                 (cond
                                   (force-string
                                    (let ((body (uiop:slurp-input-stream :string
                                                                     (dex:response-body e))))
                                      (make-string-input-stream
                                       body)))
                                   (t
                                    (flexi-streams:make-in-memory-input-stream
                                     (flexi-streams:string-to-octets
                                      "<html><body>404 Not found</body></html>"))))
                                 (dex:response-status e)
                                 (dex:response-headers e))))))
             (log:info "Fetching: ~a" url)
             (format *replay-logs* "Fetching: ~a~%" url)
             (finish-output *replay-logs*)
             (dex:get url :want-stream t :force-binary force-binary
                          :force-string force-string))
         (setf (gethash "X-Original-Url" response-headers) (quri:render-uri url))
         (values remote-stream status response-headers)))
      (t
       (error "unsupported scheme: ~a" scheme)))))

(let ((cache-dir))
 (defun http-cache-dir ()
   (util:or-setf
    cache-dir
    (tmpdir:mkdtemp))))

(defun read-file (file)
  (with-open-file (s file :direction :input)
    (read s)))

(defun write-to-file (form file)
  (with-open-file (s file :direction :output :if-exists :supersede)
    (write form :stream s)))

(defun http-get (url &key (force-binary t)
                       (cache t))
  (let ((cache-key (format
                    nil "~a-~a-v4"
                    (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 (flexi-streams:string-to-octets  (quri:render-uri url))))
                    force-binary)))
    (let* ((output (make-pathname :name cache-key :type "data" :defaults (http-cache-dir)))
           (status (make-pathname :type "status" :defaults output))
           (headers (make-pathname :type "headers" :defaults output)))
      (flet ((read-cached ()
               (values
                (open output :element-type (if force-binary
                                               '(unsigned-byte 8)
                                               'character))
                (read-file status)
                (a:alist-hash-table
                 (read-file headers))))
             (good-cache-p (file)
               (and
                (uiop:file-exists-p file)
                (> (file-write-date file)
                   (- (get-universal-time) 3600)))))
        (cond
          ((and cache (every #'good-cache-p (list output status headers)))
           (format *replay-logs* "Using cached asset for ~a~%" url)
           (read-cached))
         (t
          ;; we're not cached yet
          (multiple-value-bind (stream %status %headers)
              (http-get-without-cache url :force-binary t)
            (with-open-file (output output :element-type '(unsigned-byte 8)
                                           :if-exists :supersede
                                           :direction :output)
              (uiop:copy-stream-to-stream stream output :element-type '(unsigned-byte 8)))
            (write-to-file %status status)
            (write-to-file (a:hash-table-alist %headers) headers))
          (read-cached)))))))

(defmethod fetch-asset (url tmpdir (snapshot snapshot))
  "Fetches the url into a file <hash>.<file-type> in the tmpdir."
  (let ((pathname (ignore-errors (quri:uri-file-pathname url))))
   (restart-case
       (call-with-fetch-asset
        (lambda ()
          (http-get url))
        (cond
          (pathname
           (pathname-type pathname))
          (t
           nil))
        tmpdir
        :url url
        :snapshot snapshot)
     (retry-fetch-asset ()
       (fetch-asset url tmpdir snapshot)))))

(defun regexs ()
  ;; taken from https://github.com/callumlocke/css-url-rewriter/blob/master/lib/css-url-rewriter.js
  (uiop:read-file-lines
   (asdf:system-relative-pathname :screenshotbot.pro "replay-regex.txt")))

(defun rewrite-css-urls (css fn)
  (destructuring-bind (property-matcher url-matcher) (regexs)
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
             ((str:starts-with-p "data:" url)
              ;; we never wan't to rewrite data urls
              url)
             (t
              (format nil "url(~a)" (funcall fn url))))))))))


(defmethod fetch-css-asset ((snapshot snapshot) url tmpdir)
  (multiple-value-bind (remote-stream status response-headers) (http-get url :force-binary nil)
    (with-open-stream (remote-stream remote-stream)
     (uiop:with-temporary-file (:stream out :pathname p :type "css"
                                :directory (tmpdir snapshot))
       (flet ((rewrite-url (this-url)
                (let ((full-url (quri:merge-uris this-url url)))
                  (asset-file
                   (push-asset snapshot full-url nil)))))
         (let* ((css (uiop:slurp-stream-string remote-stream))
                (css (rewrite-css-urls css #'rewrite-url)))
           (write-string css out)
           (finish-output out)))
       (write-asset p "css"
                    :tmpdir (tmpdir snapshot)
                    :url url
                    :status status
                    :snapshot snapshot
                    :stylesheetp t
                    :response-headers response-headers)))))

(defmethod push-asset ((snapshot snapshot) url stylesheetp)
  (let ((stylesheetp (or stylesheetp (str:ends-with-p ".css" (quri:render-uri url)))))
    (loop for asset in (assets snapshot)
          if (string=(url asset) (quri:render-uri url))
            do
             (return-from push-asset asset)
          finally
             (let ((asset (cond
                            (stylesheetp
                             (fetch-css-asset snapshot url (tmpdir snapshot)))
                            (t
                             (fetch-asset url (tmpdir snapshot) snapshot)))))
               (push asset (assets snapshot))
               (return asset)))))

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

(defmethod process-node ((node plump:element) snapshot root-url)
  (let ((name (plump:tag-name node)))
    (labels ((? (test-name)
               (string-equal name test-name))
             (safe-replace-attr (name fn)
               "Replace the attribute if it exists with the output from the function"
               (let ((val (plump:attribute node name)))
                 (when (and val
                            (not (str:starts-with-p "data:" val)))
                   (let* ((ref-uri val)
                          (uri (quri:merge-uris ref-uri root-url)))
                    (setf (plump:attribute node name) (funcall fn uri))))))
             (replace-attr (name &optional stylesheetp)
               (safe-replace-attr
                name
                (lambda (uri)
                  (let* ((asset (push-asset snapshot uri stylesheetp))
                         (res (asset-file asset)))
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
                                     for uri = (quri:merge-uris url root-url)
                                     if (<= (parse-intrinsic width) max-width)
                                       collect
                                       (let ((asset (push-asset snapshot uri nil)))
                                         (str:join " "
                                                   (list
                                                    (asset-file asset)
                                                    width)))))))
                        (setf (plump:attribute node name)
                              final-attr))))))))
      (cond
       ((or (? "img")
            (? "source"))
        (replace-attr "src")
        (replace-srcset "srcset")
        ;; webpack? Maybe?
        ;;(replace-attr "data-src")
        ;;(replace-srcset "data-srcset")
        (plump:remove-attribute node "decoding")
        (plump:remove-attribute node "loading")
        (plump:remove-attribute node "data-gatsby-image-ssr"))
       ((? "picture")
        (replace-srcset "srcset"))
       ((? "iframe")
        (setf (plump:attribute node "src")
              "/iframe-not-supported"))
       ((? "video")
        ;; autoplay videos mess up screenshots
        (plump:remove-attribute node "autoplay"))
       ((? "link")
        (let ((rel (plump:attribute node "rel")))
         (cond
           ((string-equal "canonical" rel)
            ;; do nothing
            (values))
           (t
            (replace-attr "href" (string-equal "stylesheet" rel))))))
       ((? "script")
        (replace-attr "src")))))
  (call-next-method))

(defmethod process-node ((node plump:nesting-node) snapshot root-url)
  (loop for child across (plump:children node)
        do (process-node child snapshot root-url))
  (call-next-method))

(defmethod process-node :before (node snapshot root-url)
  ;;(log:info "Looking at: ~S" node)
  )

(defun add-css (html)
  "Add the replay css overrides to the html"
  (a:when-let (head (ignore-errors (elt (lquery:$ html "head") 0)))
    (let ((link (plump:make-element
                        head "link")))
      (setf (plump:attribute link "href") "/css/replay.css")
      (setf (plump:attribute link "rel") "stylesheet"))))

(defun load-url-into (snapshot url tmpdir)
  (restart-case
      (let* ((content (http-get-without-cache url :force-string t
                                                  :force-binary nil))
             (html (plump:parse content)))
        (process-node html snapshot url)
        (add-css html)

        #+nil(error "got html: ~a"
                    (with-output-to-string (s)
                      (plump:serialize html s)))
        (uiop:with-temporary-file (:direction :io :stream tmp :element-type '(unsigned-byte 8))
          (let ((root-asset (call-with-fetch-asset
                             (lambda ()
                               (plump:serialize html (flexi-streams:make-flexi-stream tmp :element-type 'character :external-format :utf-8))
                               (file-position tmp 0)
                               (let ((headers (make-hash-table)))
                                 (setf (gethash "content-type" headers)
                                       "text/html; charset=UTF-8")
                                 (values tmp 200 headers)))
                             "html"
                             tmpdir
                             :url url
                             :snapshot snapshot)))
            (push (asset-file root-asset)
                  (root-files snapshot))
            (push root-asset
                  (assets snapshot))))
        snapshot)
    (retry-load-url-into ()
      (load-url-into snapshot url tmpdir))))

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
    (load-url-into snapshot url tmpdir)))

;; (render "https://www.rollins.edu/college-of-liberal-arts")
