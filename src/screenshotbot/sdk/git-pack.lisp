;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/git-pack
  (:use #:cl)
  (:import-from #:alexandria
                #:assoc-value
                #:when-let)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:util/health-check
                #:def-health-check)
  (:import-from #:util/request
                #:http-request)
  (:local-nicknames (#:git #:screenshotbot/sdk/git)))
(in-package :screenshotbot/sdk/git-pack)

(defmacro assert-equal (one two)
  `(let ((one ,one)
         (two ,two))
     (unless (equal one two)
       (error "Expected ~a to be equal to ~a, but got ~a"
              ',two
              one
              two))))

(defclass abstract-upload-pack ()
  ())

(defclass upload-pack (abstract-upload-pack)
  ((stream :initarg :stream
           :reader %stream)))

(defclass http-upload-pack (abstract-upload-pack)
  ((repo :initarg :repo
         :reader http-url)
   (extra-headers :initarg :extra-headers
                  :initform nil
                  :reader extra-headers)
   (stream :initarg :stream
           :accessor %stream)))

(defparameter +git@-regex+ "^([a-zA-Z0-9]*)@([a-zA-Z0-9.]*):(.*)$")
(defparameter +ssh//-regex+ "^ssh://([a-zA-Z0-9]*)@([a-zA-Z0-9.]*)(:([0-9]+))?(/.*)$")

(defun supported-remote-repo-p (repo)
  (and
   (or
    (cl-ppcre:scan-to-strings +git@-regex+ repo)
    (cl-ppcre:scan-to-strings +ssh//-regex+ repo)
    (str:starts-with-p "https:" repo)
    (str:starts-with-p "http:" repo))
   (not
    ;; T1892
    (str:containsp "ssh.dev.azure.com" repo))))

(defun get-ssh-parts (url)
  "Returns four arguments: the user, the host, the directory and finally the port"
  (multiple-value-bind (match parts)
      (cl-ppcre:scan-to-strings +git@-regex+ url)
    (when match
      (return-from get-ssh-parts (values
                                  (elt parts 0)
                                  (elt parts 1)
                                  (elt parts 2)))))
  (multiple-value-bind (match parts)
      (cl-ppcre:scan-to-strings +ssh//-regex+ url)
    (when match
      (return-from get-ssh-parts
        (values
         (elt parts 0)
         (elt parts 1)
         (elt parts 4)
         (elt parts 3))))))

(defmethod make-upload-pack-command ((repo string))
  (multiple-value-bind (user host directory port)
      (get-ssh-parts repo)
   (cond
     ((and user host)
      (remove-if #'null
                 (list
                  "/usr/bin/env"
                  "ssh"
                  (format nil "~a@~a" user host)
                  (when port "-p")
                  (when port port)
                  "git-upload-pack"
                  (format nil "'~a'" directory))))
     (t
      (list
       "/usr/bin/env"
       "git-upload-pack"
       repo)))))

(defmethod make-upload-pack-command ((repo git:git-repo))
  (make-upload-pack-command (namestring (git:repo-dir repo))))

(defun local-upload-pack (repo)
  (let ((cmd (make-upload-pack-command repo)))
    (log:debug "Upload pack command is: ~a" cmd)
    (multiple-value-bind (stream)
        (sys:run-shell-command
         cmd
         :output :stream
         :input :stream
         :wait nil
         :element-type '(unsigned-byte 8))
      (assert stream)
      
      (make-instance 'upload-pack
                     :stream stream))))

(defmethod make-remote-upload-pack ((repo git:git-repo))
  (let ((repo-url (git:get-remote-url repo)))
    (cond
     ((or
       (str:starts-with-p "http://" repo-url)
       (str:starts-with-p "https://" repo-url))
      (make-instance 'http-upload-pack
                     :extra-headers (git:extra-header repo)
                     :repo repo-url))
     (t
      (local-upload-pack
       repo-url)))))

(defmethod close-upload-pack (self)
  (close (%stream self)))

(defun read-length (self)
  (let ((len (make-array 4 :element-type '(unsigned-byte 8))))
    (assert (= 4 (read-sequence len (%stream self))))
    (log:trace "Got length: ~a" len)
    (parse-integer (flex:octets-to-string len)
                   :radix 16)))

(defun write-length (self len)
  (let ((len (str:downcase (format nil "~4,'0x" len))))
    (write-sequence (flex:string-to-octets len) (%stream self))))

(defmethod read-protocol-line ((self abstract-upload-pack))
  (let ((len (read-length self)))
    (cond
      ((eql 0 len)
       nil)
      (t
       (let ((content (make-array (- len 4) :element-type '(unsigned-byte 8))))
         (read-sequence content (%stream self))
         (prog1
             (flex:octets-to-string content)))))))

(defmethod read-headers (self)
  ;;  () - Got line:829e9b346306604777c9dcc2c09b61aa30511446 HEAD ... multi_ack thin-pack side-band side-band-64k ofs-delta shallow deepen-since deepen-not deepen-relative no-progress include-tag multi_ack_detailed symref=HEAD:refs/heads/new-pr filter object-format=sha1 agent=git/2.39.5
  ;; In particular we care about the filter capability in the future.

  (let ((features))
    (let ((refs
           (loop for line = (read-protocol-line self)
                 while line
                 collect
                 (destructuring-bind (ref rest)
                     (str:split " " (str:trim line) :limit 2)
                   (cond
                     (features
                      (list ref rest))
                     (t
                      (let ((parts (str:split #\Null rest)))
                        (setf features (str:split " " (second parts)))
                        (list ref (first parts)))))))))
      (values refs features))))


(defmethod write-packet (self fmt &rest content)
  (log:trace "Writing packet ~a" (apply #'format nil fmt content))
  (let ((bytes (flex:string-to-octets (apply #'format nil fmt content))))
    (let ((len (+ 5 (length bytes))))
      (write-length self len)
      (write-sequence bytes (%stream self))
      (write-byte 10 (%stream self)))
    (force-output (%stream self))))

(defmethod want (self commit)
  (write-packet self "want ~a" commit))

(defmethod write-flush (self)
  (write-length self 0)
  (force-output (%stream self)))

(defun decode-uint32 (stream)
  "Taken from bknr.datastore"
  (logior (ash (read-byte stream) 24)
          (ash (read-byte stream) 16)
          (ash (read-byte stream) 08)
          (read-byte stream)))

(defun read-packfile-header (stream)
  "Reads the header and returns the number of entries"
  ;; https://github.com/git/git/blob/master/Documentation/gitformat-pack.adoc
  (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
    (assert-equal 4 (read-sequence magic stream))
    (assert-equal "PACK" (flex:octets-to-string magic)))

  (let ((version (make-array 4 :element-type '(unsigned-byte 8))))
    (assert (= 4 (read-sequence version stream)))
    (assert (equalp #(0 0 0 2) version)))

  (decode-uint32 stream))

(defun p (x)
  (log:trace "Got value: ~a" x)
  x)

(defun read-entry-header (stream)
  "Returns two values, the type and length"
  (let ((byte (read-byte stream))
        (size 0)
        (type))
    ;; https://github.com/git/git/blob/master/packfile.c#L111-header3
    (log:trace "First byte: ~a" byte)
    (setf type (logand 7 (ash byte -4)))
    (setf size (logand byte #b1111))

    (let ((mult 4))
     (loop while (> (logand byte 128) 0)
           do
              (setf byte (read-byte stream))
              (log:trace "Read another byte: " byte)
              (incf size (p (ash (p (logand (1- #x80) byte)) mult)))
              (incf mult 7)))

    (log:trace "final size: ~a" size)
    (values type size)))

(defun simulate ()

  ;; https://github.com/git/git/blob/master/Documentation/gitprotocol-pack.adoc
  (setf *p* (local-upload-pack "/home/arnold/builds/web/.git/"))
  (read-headers *p*)
  (want *p* "ff6dbd2b1e3db2d5a89b30fe8d5eacc2a1874de3 filter")
  ;; This requires something like git config --global --add  uploadpack.allowFilter 1
  ;; Unclear if this is always available.
  ;; https://github.com/git/git/blob/master/Documentation/gitprotocol-capabilities.adoc#filter
  ;; We should check if the capability is available before attempting it.
  ;; (write-packet *p* "filter object:type=commit")
  (write-flush *p*)
  (write-packet *p* "done")
  (finish-output (%stream *p*))
  (read-protocol-line *p*)
  ;; From here on the rest is the packfile
  )

(defconstant +obj-ref-delta+ 7)
(defconstant +obj-obs-delta+ 6)

(defun read-packfile-entry (packfile)
  "Returns type and the contents of the entry"
  ;; https://github.com/git/git/blob/master/Documentation/gitprotocol-pack.adoc
  (log:trace "Reading packfile entry")
  (let ((stream (%stream packfile)))
    (multiple-value-bind (type length) (read-entry-header stream)
      (log:trace "Got type: ~a" type)

      (when (eql +obj-ref-delta+ type)
        (let ((arr (make-array 20 :element-type '(unsigned-byte 8))))
          ;; Read the name of the base object
          (read-sequence arr stream)))

      (when (eql +obj-obs-delta+ type)
        ;; I haven't encountered this in testing. It looks like this
        ;; is going to be a variable length integer. For my purposes,
        ;; I don't care what the actual value of this integer is. In
        ;; theory read-packfile-header reads the exact number of
        ;; bytes, so I'll just use that here to ignore the bytes.
        (warn "Untested code for obj-obs-delta")
        (read-entry-header (%stream packfile)))

      (let ((body (semz.decompress:decompress :zlib stream
                                              :allow-overreads-p nil
                                              :output-size length)))

        (values type
                body
                length)))))

(defun packfile-test ()
  (simulate)
  (setf *arr* (make-array 4 :element-type '(unsigned-byte 8)))
  (let ((num (read-packfile-header (%stream *p*))))
    (loop for i below num
          collect
             (multiple-value-list
              (read-packfile-entry *p*))))

  
  (close-upload-pack *p*))

(defun compute-sha1-from-commit (body)
  (let ((digest (ironclad:make-digest 'ironclad:sha1)))
    (ironclad:digest-sequence
     digest
     (flex:string-to-octets (format nil "commit ~a~a" (length body) #\Null)))
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence
      digest
      body))))

(defun parse-parents (commit)
  (let ((stream (make-string-input-stream commit)))
    (loop for line = (read-line stream)
          until (str:emptyp (str:trim line))
          for parts = (str:split " " line)
          if (equal "parent" (first parts))
            collect (second parts))))

(defun remote-ref-equals (local-ref ref)
  (let ((parts (str:split "/" ref :limit 3)))
    (and
     (eql 3 (length parts))
     (equal local-ref (elt parts 2)))))

(def-easy-macro with-opened-upload-pack (upload-pack &key &binding headers &binding features &fn fn)
  (multiple-value-bind (headers features)
      (read-headers upload-pack)
    (unwind-protect
         (fn headers features)
      #+nil
      (close-upload-pack upload-pack))))

(defun assert-content-type (http-headers expected-content-type)
  (let ((content-type (assoc-value http-headers :content-type)))
    (unless (equal expected-content-type content-type)
      (error "Got bad content-type: ~a" content-type))))

(defun remove-auth-from-uri (uri)
  (let* ((uri (quri:uri uri))
         (user (quri:uri-userinfo uri)))
    (setf (quri:uri-userinfo uri) nil)
    (values
     (quri:render-uri uri)
     (when user
      (str:split ":" user :limit 2)))))

(auto-restart:with-auto-restart (:retries 2)
 (defmethod git-http-request ((p http-upload-pack)
                              url &rest args)
   (multiple-value-bind (git-url auth)
       (remove-auth-from-uri (http-url p))
    (apply #'http-request (format nil "~a~a"
                                  (str:ensure-suffix "/" git-url)
                                  url)
           :basic-authorization (or
                                 auth
                                 (read-netrc
                                  (quri:uri-host
                                   (quri:uri (http-url p)))))
           :additional-headers (extra-headers p)
           :want-stream t
           :ensure-success t
           :read-timeout 45
           :connection-timeout 15
           :force-binary t
           args))))

(auto-restart:with-auto-restart ()
  (defmethod read-commits ((p http-upload-pack)
                           &key (filter-blobs t)
                             (depth 1000)
                             wants
                             (haves nil)
                             (parse-parents nil))
    (multiple-value-bind (stream code http-headers)
        (git-http-request
         p
         "info/refs?service=git-upload-pack"
         :method :get)
      (declare (ignore code))
      (assert-content-type http-headers "application/x-git-upload-pack-advertisement")
      (setf (%stream p) stream)
      (let ((service-headers (read-headers p)))
        ;; This is typically a single line header ... but I'm mostly
        ;; just discarding this for now.
        (log:debug "Got service headers: ~a" service-headers))
      (multiple-value-bind (headers features)
          (read-headers p)
        (close (%stream p))
        (let ((wants (calculate-wants headers features wants)))
          (cond
            ((not wants)
             ;; We're done
             nil)
            (t
             (log:debug "Wanting: ~s" wants)
             (let ((body-builder (flex:make-in-memory-output-stream)))
               (setf (%stream p) body-builder)
               (write-wants-and-haves
                p
                wants
                haves :depth depth :filter-blobs filter-blobs :features features
                      :http? t)
               (let ((bytes (flex:get-output-stream-sequence body-builder)))
                (multiple-value-bind (stream code http-headers)
                    (git-http-request p
                                      "git-upload-pack"
                                      :content-type "application/x-git-upload-pack-request"
                                      :method :post
                                      :content bytes)
                  (assert-content-type http-headers "application/x-git-upload-pack-result")
                  (setf (%stream p) stream)
                  (when depth
                    (read-shallow-lines p))
                  (read-response-and-packfile p :parse-parents parse-parents)))))))))))

(defmethod read-commits :around ((p abstract-upload-pack)
                                 &rest args
                                 &key wants
                                 &allow-other-keys)
  (let ((wants
          (cond
            ((listp wants)
             (lambda (list)
               (loop for (sha . remote-ref) in list
                     if (loop for local-ref in wants
                              if (remote-ref-equals local-ref remote-ref)
                                return t)
                       collect sha)))
            (t
             wants))))
    (apply #'call-next-method p :wants wants args)))

(defun calculate-wants (headers features wants)
  (log:debug "Got features: ~a" features)
  (unless (str:s-member features "allow-tip-sha1-in-want")
    (warn "Git server doesn't support allow-tip-sha1-in-want, features: ~s" features))
  
  (unless (or
           (str:s-member features "allow-tip-sha1-in-want")
           (str:s-member features "allow-reachable-sha1-in-want"))
    (error "This git server doesn't support allow-{tip|reachable}-sha1-in-want"))
  (funcall
   wants
   (loop for this-branch in headers
         collect (cons
                  (first this-branch) (second this-branch)))))

(defun read-shallow-lines (p)
  "In SSH this happens after wants/deepen and before haves. In HTTP this happens after wants/deepen/haves."
  (log:debug "Reading shallow lines")
  (loop for line = (read-protocol-line p)
        while line
        do (log:debug "Ignoring: ~a" line)))

(defun write-wants-and-haves (p wants haves &key depth filter-blobs features http?)
  (want p (format nil "~a filter allow-tip-sha1-in-want allow-reachable-sha1-in-want shallow agent=screenshotbot-cli" (car wants)))
  (dolist (want (cdr wants))
    (want p want))

  (when depth
    (write-packet p "deepen ~a" depth))

  (cond
    ((and
      filter-blobs
      (str:s-member features "filter"))
     (log:debug "filter is enabled, sending filter")
     (write-packet p "filter blob:none"))
    (t
     (warn "Filter is not supported")))
  (write-flush p)

  (when (and depth (not http?))
    ;; These should be "shallow ~a" or "unshallow ~a" lines
    ;; We don't care for them really
    (read-shallow-lines p))

  ;; We must remove duplicate haves, otherwise upload-pack misbehaves
  ;; in the response
  (fset:do-set (have (fset:convert 'fset:set haves))
    (write-packet p "have ~a" have))
  (when (and haves (not http?))
    (write-flush p))

         
  (write-packet p "done")
  (finish-output (%stream p)))

(defmethod read-commits ((p upload-pack)
                         &key (filter-blobs t)
                           (depth 1000)
                           wants
                           (haves nil)
                           (parse-parents nil))
  "WANTS is a function that take an alist of (sha -> ref-name) and
returns all the SHAs it wants. If this function is called, we can
assume that the server has allow-{tip|reachable}-sha1-in-want enabled.

Returns two values: An alist of commit hashes to commit bodys, and as
a second value the headers that were initially provided (sha and refs)
"
  (with-opened-upload-pack (p :headers headers :features features)
    ;;(log:trace "Got headers: ~S" headers)
    (let ((wants
            (calculate-wants headers features wants)))
      (cond
        ((not wants)
         (write-flush p))
        (t
         (write-wants-and-haves
          p
          wants
          haves :depth depth :filter-blobs filter-blobs :features features)

         (log:debug "Waiting for response")
         (read-response-and-packfile
          p :parse-parents parse-parents))))))

(defun read-response-and-packfile (p &key parse-parents)
  ;; This should either be a NAK (nothing common found), or ACK
  ;; <commit> signalling that that was a common commit.
  (let ((line (read-protocol-line p)))
    (log:debug "Got ACK/NAK: ~a" line)
    (unless (or
             (str:starts-with-p "NAK" line)
             (str:starts-with-p "ACK" line))
      (error "Didn't get an ACK or NAK: ~a" line)))

  (log:debug "reading packfile")        
  ;; Now we get the packfile
  (let ((num (read-packfile-header (%stream p))))
    (log:debug "Packfile entries: ~a" num)
    (serapeum:collecting
      (loop for i below num
            do
               (multiple-value-bind (type body) (read-packfile-entry p)
                 (when (eql 1 type)
                   (collect
                       (cons
                        (compute-sha1-from-commit body)
                        (cond
                          (parse-parents
                           (parse-parents (flex:octets-to-string body)))
                          (t
                           (flex:octets-to-string body)))))))))))

(defun read-netrc-line (stream)
  (when-let ((line (read-line stream nil nil)))
   (let ((line (str:trim line)))
     (cond
       ((str:starts-with-p "#" line)
        ;; Technically this might not really be needed
        (read-netrc-line stream))
       (t
        (let ((parts (str:split " " line)))
          (values
           (str:trim (first parts))
           (str:trim (car (last parts))))))))))

(defun read-netrc (host &key (pathname "~/.netrc"))
  (when (path:-e pathname)
   (let (login
         password
         found)
     (block nil
       (flet ((maybe-return ()
                (when (and found login password)
                  (return-from read-netrc (list login password)))))
         (with-open-file (s pathname :direction :input)
           (dotimes (i 10000) ;; avoid bugs
             (multiple-value-bind (key value) (read-netrc-line s)
               (cond
                 ((not key)
                  (return nil))
                 ((and
                   (not found)
                   (equal "machine" key)
                   (equal host value))
                  (setf found t))
                 ((and
                   found
                   (equal "machine" key))
                  (return nil))
                 ((and
                   found
                   (equal "login" key))
                  (setf login value)
                  (maybe-return))
                 ((and
                   found
                   (equal "password" key))
                  (setf password value)
                  (maybe-return)))))))))))


(defmethod read-commits ((repo string) &rest args &key &allow-other-keys)
  (cond
    ((or
      (str:starts-with-p "http:" repo)
      (str:starts-with-p "https://" repo))
     (apply #'read-commits (make-instance 'http-upload-pack
                                          :repo repo)
            args))
    (t
     (let* ((p (local-upload-pack repo)))
       (apply #'read-commits p args)))))

;; (log:config :warn)
;; (read-commits "/home/arnold/builds/fast-example/.git" :branch "refs/heads/master")
;; (length (read-commits "git@github.com:tdrhq/fast-example.git" :branch "refs/heads/master" :haves (list "3c6fcd29ecdf37a2d1a36f46309787d32e11e69b")))
;; (length (read-commits "git@github.com:tdrhq/fast-example.git" :wants (list "master") :depth 30))
;; (read-commits "https://github.com/tdrhq/fast-example.git" :wants (list "master") :haves (list "3c6fcd29ecdf37a2d1a36f46309787d32e11e69b") :depth 300)
;; (read-commits "https://github.com/tdrhq/alisp.git" :wants (list "master") :depth nil)
;; (read-commits "git@gitlab.com:tdrhq/fast-example.git" :wants (list "master") :depth 30 :haves (list "3c6fcd29ecdf37a2d1a36f46309787d32e11e69b" "3c6fcd29ecdf37a2d1a36f46309787d32e11e69b"))
;; (read-commits "git@github.com:tdrhq/fast-example.git" :wants (list "master") :depth 30 :haves (list "3c6fcd29ecdf37a2d1a36f46309787d32e11e69b"  "3be209da6b797a6821d8c43b13c8b2ce64cf4a5b"))
;; (read-commits "git@bitbucket.org:tdrhq/fastq-example.git" :wants (list "master") :depth 30)
;; (read-commits (make-remote-upload-pack (make-instance 'git:git-repo :dir "/tmp/alisp/")) :wants (list "master"))

;; Azure features: ( multi_ack thin-pack side-band side-band-64k no-progress multi_ack_detailed no-done shallow allow-tip-sha1-in-want filter symref=HEAD:refs/heads/master)
;; Azure also requires clients to support multi-ack :/
;; (read-commits "git@ssh.dev.azure.com:v3/testsbot/fast-example/fast-example" :wants (list "master") :depth 30)
;; (length (read-commits "git@github.com:tdrhq/fast-example.git" :wants (list "master") :depth 30))
;; (length (read-commits "git@github.com:tdrhq/braft.git" :branch "master" :parse-parents t))
;; (read-commits "ssh://git@phabricator.tdrhq.com:2222/source/web.git" :wants (list "master") :extra-wants (list "4ec592fc02c8bf200e4e2b9902e9a71fd8de3aec") :depth 100)
;; (read-commits "/home/arnold/builds/web/.git" :wants (list "master") :depth 2)
;; (read-commits "https://github.com/tdrhq/alisp.git" :git-config "/tmp/alisp/.git/config" :wants (list "master") :depth nil)

(def-health-check can-compute-sha1 ()
  "The first time we tried to do this, sha1 was removed out of the delivered image"
  (assert (equal "e502bf251eaf300073dde00c0a39bd1061fb04de" (compute-sha1-from-commit
                                                             (flex:string-to-octets "hello world")))))



;; (git:extra-header (make-instance 'git:git-repo :dir "/tmp/alisp/"))
