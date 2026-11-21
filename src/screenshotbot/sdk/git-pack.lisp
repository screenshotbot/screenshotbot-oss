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
                #:not-null!
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
  ((multi-ack-p :initarg :multi-ack-p
                :initform t
                :reader multi-ack-p
                :documentation "Whether the server supports multi_ack capability")))

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
    (str:starts-with-p "http:" repo))))

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

(defmethod make-upload-pack-for-local-repo-only ((repo git:git-repo))
  (make-upload-pack-from-cmd
   (list
    "/usr/bin/env"
    "git-upload-pack"
    (namestring (git:repo-dir repo)))))

(defmethod make-upload-pack-command ((repo git:git-repo))
  (make-upload-pack-command (namestring (git:repo-dir repo))))

(defun local-upload-pack (repo)
  "TODO: badly named function. See also make-upload-pack-for-local-repo-only."
  (let ((cmd (make-upload-pack-command repo)))
    (log:debug "Upload pack command is: ~a" cmd)
    (make-upload-pack-from-cmd cmd)))

(defun make-upload-pack-from-cmd (cmd)
  (multiple-value-bind (stream)
      (sys:run-shell-command
       cmd
       :output :stream
       :input :stream
       :wait nil
       :element-type '(unsigned-byte 8))
    (assert stream)
      
    (make-instance 'upload-pack
                   :stream stream)))

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
       (not-null! repo-url))))))

(defmethod close-upload-pack (self)
  (close (%stream self)))

(defun read-length (self)
  (let ((len (make-array 4 :element-type '(unsigned-byte 8))))
    (let ((num-bytes (read-sequence len (%stream self))))
     (unless (= 4 num-bytes)
       (error "Could not read length, got ~a bytes" num-bytes)))

    (when (equalp len #. (flex:string-to-octets "PACK"))
      (error 'pack-header-encountered))
    
    (log:trace "Got length: ~a" len)
    (parse-integer (flex:octets-to-string len)
                   :radix 16)))

(defun write-length (self len)
  (let ((len (str:downcase (format nil "~4,'0x" len))))
    (write-sequence (flex:string-to-octets len) (%stream self))))

(define-condition pack-header-encountered (error)
  ())

(define-condition upload-pack-error (error)
  ((message :initarg :message
            :reader upload-pack-error-message))
  (:report (lambda (condition stream)
             (format stream "Git upload-pack error: ~a"
                     (upload-pack-error-message condition)))))

(defmethod read-protocol-line ((self abstract-upload-pack))
  (let ((len (read-length self)))
    (cond
      ((eql 0 len)
       nil)
      (t
       (let ((content (make-array (- len 4) :element-type '(unsigned-byte 8))))
         (read-sequence content (%stream self))
         (let ((result
                 (flex:octets-to-string content)))
           (log:trace "Read protocol line: ~a" result)
           ;; Check for ERR packets sent by git
           (when (str:starts-with-p "ERR " result)
             (error 'upload-pack-error
                    :message (str:substring 4 nil result)))
           result))))))

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

(defun read-packfile-header (stream &key (read-magic-p t))
  "Reads the header and returns the number of entries

If READ-MAGIC-P is true, we'll read the first four bytes of PACK magic."
  ;; https://github.com/git/git/blob/master/Documentation/gitformat-pack.adoc
  (when read-magic-p
    (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
      (assert-equal 4 (read-sequence magic stream))
      (assert-equal "PACK" (flex:octets-to-string magic))))

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

;; Pack object types, see gitformat-pack.adoc
(defconstant +obj-commit+ 1)
(defconstant +obj-ofs-delta+ 6)
(defconstant +obj-ref-delta+ 7)

(defclass counting-input-stream (stream:fundamental-binary-input-stream)
  ((source :initarg :source
           :reader counting-source)
   (pos :initform 0
        :accessor counting-position
        :documentation "Number of bytes read from SOURCE through this stream."))
  (:documentation "Wraps a binary input stream and counts the bytes read
through it, so that we know the offset of each packfile entry. OFS_DELTA
entries refer to their base by offset."))

(defmethod stream-element-type ((self counting-input-stream))
  '(unsigned-byte 8))

(defmethod stream:stream-read-byte ((self counting-input-stream))
  (let ((byte (read-byte (counting-source self) nil :eof)))
    (unless (eql :eof byte)
      (incf (counting-position self)))
    byte))

(defmethod stream:stream-read-sequence ((self counting-input-stream) sequence start end)
  (let ((new-end (read-sequence sequence (counting-source self)
                                :start start :end end)))
    (incf (counting-position self) (- new-end start))
    new-end))

(defun read-ofs-delta-offset (stream)
  "Reads the negative base offset of an OFS_DELTA entry. This is not the
same varint as the entry header: each continuation adds one before
shifting. See get_delta_base() in packfile.c."
  (let* ((byte (read-byte stream))
         (offset (logand byte #x7f)))
    (loop while (logbitp 7 byte)
          do
             (setf byte (read-byte stream))
             (setf offset (logior (ash (1+ offset) 7)
                                  (logand byte #x7f))))
    offset))

(defun read-packfile-entry (stream)
  "Reads one entry from the packfile STREAM, which must be a
COUNTING-INPUT-STREAM.

Returns four values: the type, the decompressed body, the offset of
the entry in the pack and the delta base. For REF_DELTA entries the
base is the base object's SHA as a hex string, and for OFS_DELTA it is
the offset of the base entry in the pack. The body of a delta entry is
the delta data, not the object."
  ;; https://github.com/git/git/blob/master/Documentation/gitformat-pack.adoc
  (log:trace "Reading packfile entry")
  (let ((offset (counting-position stream)))
    (multiple-value-bind (type length) (read-entry-header stream)
      (log:trace "Got type: ~a" type)
      (let ((base
              (cond
                ((eql +obj-ref-delta+ type)
                 (let ((arr (make-array 20 :element-type '(unsigned-byte 8))))
                   (assert-equal 20 (read-sequence arr stream))
                   (ironclad:byte-array-to-hex-string arr)))
                ((eql +obj-ofs-delta+ type)
                 (- offset (read-ofs-delta-offset stream))))))
        (let ((body (semz.decompress:decompress :zlib stream
                                                :allow-overreads-p nil
                                                :output-size length)))
          (values type
                  body
                  offset
                  base))))))

(defun read-delta-size (delta pos)
  "Reads the little-endian size varint at the start of delta data.
Returns the size and the position after it."
  (let ((size 0)
        (shift 0))
    (loop
      (let ((byte (aref delta pos)))
        (incf pos)
        (setf size (logior size (ash (logand byte #x7f) shift)))
        (incf shift 7)
        (unless (logbitp 7 byte)
          (return (values size pos)))))))

(defun apply-delta (base delta)
  "Applies git's delta format (see patch-delta.c) to the octet vector
BASE, and returns the resulting octet vector."
  (multiple-value-bind (base-size pos) (read-delta-size delta 0)
    (unless (= base-size (length base))
      (error "Delta expects a base of ~a bytes, but the base has ~a bytes"
             base-size (length base)))
    (multiple-value-bind (result-size pos) (read-delta-size delta pos)
      (let ((result (make-array result-size :element-type '(unsigned-byte 8)))
            (out 0))
        (flet ((next-byte ()
                 (prog1 (aref delta pos)
                   (incf pos))))
          (loop while (< pos (length delta))
                do
                   (let ((op (next-byte)))
                     (cond
                       ((logbitp 7 op)
                        ;; Copy from the base
                        (let ((copy-offset 0)
                              (size 0))
                          (loop for i below 4
                                if (logbitp i op)
                                  do (setf copy-offset
                                           (logior copy-offset (ash (next-byte) (* 8 i)))))
                          (loop for i below 3
                                if (logbitp (+ 4 i) op)
                                  do (setf size
                                           (logior size (ash (next-byte) (* 8 i)))))
                          (when (zerop size)
                            (setf size #x10000))
                          (unless (and (<= (+ copy-offset size) base-size)
                                       (<= (+ out size) result-size))
                            (error "Bad delta copy instruction"))
                          (replace result base :start1 out
                                               :start2 copy-offset
                                               :end2 (+ copy-offset size))
                          (incf out size)))
                       ((plusp op)
                        ;; Insert the next OP bytes from the delta
                        (unless (and (<= (+ pos op) (length delta))
                                     (<= (+ out op) result-size))
                          (error "Bad delta insert instruction"))
                        (replace result delta :start1 out
                                              :start2 pos
                                              :end2 (+ pos op))
                        (incf out op)
                        (incf pos op))
                       (t
                        (error "Unexpected delta opcode 0"))))))
        (unless (= out result-size)
          (error "Delta produced ~a bytes, expected ~a" out result-size))
        result))))

(defun read-packfile-commits (stream num)
  "Reads NUM packfile entries from STREAM, and returns a list of (SHA
. BODY) for every commit in the pack, in pack order, including commits
that were sent as deltas.

Bases of deltas might arrive after the deltas, and deltas can be
chained, so we resolve each delta as soon as its base commit is
known. Deltas whose base never shows up are deltas of trees (or of
objects we don't have, which shouldn't happen since we don't ask for
thin packs), and are dropped."
  (let ((stream (make-instance 'counting-input-stream :source stream))
        (commits nil)
        (bodies-by-offset (make-hash-table))
        (bodies-by-sha (make-hash-table :test #'equal))
        ;; Deltas waiting on their base, keyed by the base's offset or
        ;; SHA. Each value is a list of (offset . delta-data).
        (waiting-by-offset (make-hash-table))
        (waiting-by-sha (make-hash-table :test #'equal)))
    (labels ((add-commit (offset body)
               (let ((sha (compute-sha1-from-commit body)))
                 (push (list offset sha body) commits)
                 (setf (gethash offset bodies-by-offset) body)
                 (setf (gethash sha bodies-by-sha) body)
                 (let ((waiting (append
                                 (gethash offset waiting-by-offset)
                                 (gethash sha waiting-by-sha))))
                   (remhash offset waiting-by-offset)
                   (remhash sha waiting-by-sha)
                   (loop for (delta-offset . delta) in waiting
                         do (add-commit delta-offset (apply-delta body delta))))))
             (add-delta (offset base delta)
               (let* ((table (if (stringp base) waiting-by-sha waiting-by-offset))
                      (base-body (gethash base (if (stringp base)
                                                   bodies-by-sha
                                                   bodies-by-offset))))
                 (cond
                   (base-body
                    (add-commit offset (apply-delta base-body delta)))
                   (t
                    (push (cons offset delta) (gethash base table)))))))
      (loop for i below num
            do
               (multiple-value-bind (type body offset base)
                   (read-packfile-entry stream)
                 (cond
                   ((eql +obj-commit+ type)
                    (add-commit offset body))
                   ((or (eql +obj-ref-delta+ type)
                        (eql +obj-ofs-delta+ type))
                    (add-delta offset base body)))))
      (log:debug "Unresolved deltas (probably trees): ~a"
                 (+ (hash-table-count waiting-by-offset)
                    (hash-table-count waiting-by-sha)))
      (loop for (nil sha body) in (sort commits #'< :key #'first)
            collect (cons sha body)))))

(defun read-packfile (stream &key (read-magic-p t))
  "Reads a complete packfile from STREAM, and returns an alist of
commit SHAs to commit bodies (as octets)."
  (let ((num (read-packfile-header stream :read-magic-p read-magic-p)))
    (log:debug "Packfile entries: ~a" num)
    (read-packfile-commits stream num)))

(defun packfile-test ()
  (simulate)
  (let ((num (read-packfile-header (%stream *p*))))
    (read-packfile-commits (%stream *p*) num))
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

(defmethod %get-auth-ignoring-extra-headers ((p http-upload-pack))
  (multiple-value-bind (git-url auth)
      (remove-auth-from-uri (http-url p))
    (declare (ignore git-url))
    (or
     auth
     (read-netrc
      (quri:uri-host
       (quri:uri (http-url p))))
     (git:credential-fill (http-url p)))))

(defmethod %get-auth ((p http-upload-pack))
  (let ((auth (unless (has-authorization-header-p (extra-headers p))
                (%get-auth-ignoring-extra-headers p))))
    (unless (or auth (has-authorization-header-p (extra-headers p)))
      (log:info "Could not find authentication information for Git http, assuming this is a public repo"))
    auth))

(auto-restart:with-auto-restart (:retries 2)
 (defmethod git-http-request ((p http-upload-pack)
                              url &rest args)
   (let ((git-url (remove-auth-from-uri (http-url p))))
     (let ((auth (%get-auth p)))
       (apply #'http-request (format nil "~a~a"
                                     (str:ensure-suffix "/" git-url)
                                     url)
              :basic-authorization auth
              :additional-headers (extra-headers p)
              :want-stream t
              :ensure-success t
              :read-timeout 45
              :connection-timeout 15
              :force-binary t
              args)))))

(defun has-authorization-header-p (extra-headers)
  (loop for (key . value) in extra-headers
        if (string-equal key "authorization")
          return t))

(auto-restart:with-auto-restart ()
  (defmethod read-commits ((p http-upload-pack)
                           &key (filter-blobs t)
                             (filter "tree:0")
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
                haves :depth depth :filter-blobs filter-blobs :filter filter :features features
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

(defun supports-multi-ack-p (features)
  "Check if the server supports multi_ack or multi_ack_detailed capability"
  (or (str:s-member features "multi_ack")
      (str:s-member features "multi_ack_detailed")))

(defun read-shallow-lines (p)
  "In SSH this happens after wants/deepen and before haves. In HTTP this happens after wants/deepen/haves."
  (log:debug "Reading shallow lines")
  (loop for line = (read-protocol-line p)
        while line
        do (log:debug "Ignoring: ~a" line)))

(defun write-wants-and-haves (p wants haves &key depth filter-blobs (filter "tree:0") features http?)
  "FILTER is the object filter we send if FILTER-BLOBS is set and the
server supports filters. We only need commits, so the default, tree:0,
skips trees as well as blobs; with blob:none, trees are most of the
pack.

Rarely, a server allows filters but not tree filters (git configured
with uploadpackfilter.tree.allow=false, or a git from before tree
filters). It then sends an ERR, which signals UPLOAD-PACK-ERROR, and
callers retry with blob:none. The server's capabilities only say
\"filter\", not which filters it allows, so we can't know in
advance. See T2486."
  (let* ((multi-ack-p (supports-multi-ack-p features))
         (capabilities (format nil "~a filter allow-tip-sha1-in-want allow-reachable-sha1-in-want shallow~a agent=screenshotbot-cli"
                               (car wants)
                               (if multi-ack-p " multi_ack" ""))))
    (want p capabilities))
  (dolist (want (cdr wants))
    (want p want))

  (when depth
    (write-packet p "deepen ~a" depth))

  (cond
    ((and
      filter-blobs
      (str:s-member features "filter"))
     (log:debug "filter is enabled, sending filter ~a" filter)
     (write-packet p "filter ~a" filter))
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
                           (filter "tree:0")
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
          haves :depth depth :filter-blobs filter-blobs :filter filter :features features)

         (log:debug "Waiting for response")
         (read-response-and-packfile
          p :parse-parents parse-parents))))))

(defun process-ack (p)
  "See process_ack() in fetch-pack.c"
  (loop
    (let ((line (read-protocol-line p)))
      (log:debug "Got ACK/NAK: ~a" line)
      (cond
        ((equal "NAK" (str:trim line)))
        ((str:starts-with-p "ACK " line))
        ((equal "ready" (str:trim line)))
        (t
         (error "Didn't get an ACK or NAK or recognized line: ~a" line))))))

(defun read-response-and-packfile (p &key parse-parents)
  ;; This should either be a NAK (nothing common found), or ACK
  ;; <commit> signalling that that was a common commit.
  (handler-case
      (progn
        (process-ack p)
        (error "We should never be here"))
    (pack-header-encountered (e)))
  
  (log:debug "reading packfile")
  ;; Now we get the packfile
  (loop for (sha . body) in (read-packfile (%stream p)
                                           ;; We've already read the magic in process-ack
                                           :read-magic-p nil)
        collect
        (cons
         sha
         (cond
           (parse-parents
            (parse-parents (flex:octets-to-string body)))
           (t
            (flex:octets-to-string body))))))

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
;; (read-commits "git@bitbucket.org:tdrhq/fast-example.git" :wants (list "master") :depth 30)
;; (read-commits (make-remote-upload-pack (make-instance 'git:git-repo :dir "/tmp/alisp/")) :wants (list "master"))

;; Azure features: ( multi_ack thin-pack side-band side-band-64k no-progress multi_ack_detailed no-done shallow allow-tip-sha1-in-want filter symref=HEAD:refs/heads/master)
;; Azure also requires clients to support multi-ack :/
;; (read-commits "git@ssh.dev.azure.com:v3/testsbot/fast-example/fast-example" :wants (list "master") :depth 30)
;; (length (read-commits "git@github.com:tdrhq/fast-example.git" :wants (list "master") :depth 30 :parse-parents t))
;; (length (read-commits "git@github.com:tdrhq/braft.git" :branch "master" :parse-parents t))
;; (read-commits "ssh://git@phabricator.tdrhq.com:2222/source/web.git" :wants (list "master") :extra-wants (list "4ec592fc02c8bf200e4e2b9902e9a71fd8de3aec") :depth 100)
;; (read-commits "/home/arnold/builds/web/.git" :wants (list "master") :depth 2)
;; (read-commits "https://github.com/tdrhq/alisp.git" :git-config "/tmp/alisp/.git/config" :wants (list "master") :depth nil)

;; Reproduce "fatal: git upload-pack: not our ref" error:
;; This happens when you request a SHA that doesn't exist in the repository,
;; typically because it was removed via force push or history rewrite.
;; (read-commits "git@bitbucket.org:tdrhq/fast-example.git" :wants (lambda (refs) (list "458a81c522f6d7325c9d893624c926eaa8ed2cc0")) :depth 30)

(def-health-check can-compute-sha1 ()
  "The first time we tried to do this, sha1 was removed out of the delivered image"
  (assert (equal "e502bf251eaf300073dde00c0a39bd1061fb04de" (compute-sha1-from-commit
                                                             (flex:string-to-octets "hello world")))))



;; (git:extra-header (make-instance 'git:git-repo :dir "/tmp/alisp/"))
