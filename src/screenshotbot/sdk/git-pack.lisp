;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/git-pack
  (:use #:cl)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:util/misc
                #:?.)
  (:local-nicknames (#:git #:screenshotbot/sdk/git)))
(in-package :screenshotbot/sdk/git-pack)

(defclass upload-pack ()
  ((stream :initarg :stream
           :accessor %stream)))

(defparameter +git@-regex+ "^([a-zA-Z0-9]*)@([a-zA-Z0-9.]*):(.*)$")
(defparameter +ssh//-regex+ "^ssh://([a-zA-Z0-9]*)@([a-zA-Z0-9.]*)(:([0-9]+))?(/.*)$")

(defun supported-remote-repo-p (repo)
  (or
   (cl-ppcre:scan-to-strings +git@-regex+ repo)
   (cl-ppcre:scan-to-strings +ssh//-regex+ repo)))

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
      (format nil "ssh ~a@~a~a git-upload-pack \"'~a'\""
              user
              host
              (if port (format nil " -p ~a" port) "")
              directory))
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
  (local-upload-pack
   (git:get-remote-url repo)))

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

(defmethod read-protocol-line ((self upload-pack))
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
  (let ((magick (make-array 4 :element-type '(unsigned-byte 8))))
    (assert (= 4 (read-sequence magick stream)))
    (assert (equal "PACK" (flex:octets-to-string magick))))

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

(defmethod safe-make-concatenated-stream (stream1 stream2)
  (make-concatenated-stream
   stream1
   stream2))

(defmethod safe-make-concatenated-stream (stream1 (stream2 concatenated-stream))
  (apply #'make-concatenated-stream
         stream1
         (concatenated-stream-streams stream2)))

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

      (let ((body (if (= type 1) (flex:make-in-memory-output-stream) nil))
            (dstate (chipz:make-dstate :zlib)))
        (chipz:decompress
         body
         dstate
         stream)

        (let ((body (?. flex:get-output-stream-sequence body)))
          (setf
           (%stream packfile)
           ;; TODO: we could potentially optimize this.. but do we care?
           (safe-make-concatenated-stream
            (flex:make-in-memory-input-stream
             (chipz::inflate-state-input dstate)
             :start (chipz::inflate-state-input-index dstate)
             :end (chipz::inflate-state-input-end dstate))
            (%stream packfile)))
          ;;(assert (= length (length body)))
          (values type
                  body
                  length
                  dstate))))))

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
  (let ((digest (ironclad:make-digest :sha1)))
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

(defmethod read-commits ((p upload-pack)
                         &key (filter-blobs t)
                           (depth 1000)
                           wants
                           (haves nil)
                           (parse-parents nil))
  "WANTS is a function that take a sha and a ref-name see if we want it. WANTS can
also be a list, in which case we check if the name matches.

Returns two values: An alist of commit hashes to commit bodys, and as
a second value the headers that were initially provided (sha and refs)
"
  (let ((wants
          (cond
            ((listp wants)
             (lambda (sha remote-ref)
               (declare (ignore sha))
               (loop for local-ref in wants
                     if (remote-ref-equals local-ref remote-ref)
                       return t)))
            (t
             wants))))
    (with-opened-upload-pack (p :headers headers :features features)
      (log:trace "Got headers: ~a" headers)
      (log:debug "Got features: ~a" features)
      (let ((wants
              (loop for this-branch in headers
                    if (funcall wants (first this-branch) (second this-branch))
                      collect (first this-branch))))
        (cond
          ((not wants)
           (write-flush p))
          (t
           (want p (format nil "~a filter" (car wants)))
           (dolist (want (cdr wants))
             (want p want))

           (when depth
             (write-packet p "deepen ~a" depth))

           (when (and
                  filter-blobs
                  (str:s-member features "filter"))
             (log:debug "filter is enabled, sending filter")
             (write-packet p "filter blob:none"))
           (write-flush p)

           (when depth
             ;; These should be "shallow ~a" or "unshallow ~a" lines
             ;; We don't care for them really
             (loop for line = (read-protocol-line p)
                   while line
                   do (log:trace "Ignoring: ~a" line)))
        
           (dolist (have haves)
             (write-packet p "have ~a" have))
           (when haves
             (write-flush p))

        
           (write-packet p "done")
           (finish-output (%stream p))

           (log:debug "Waiting for response")

           ;; This should either be a NAK (nothing common found), or ACK
           ;; <commit> signalling that that was a common commit.
           (read-protocol-line p)

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
                                    (flex:octets-to-string body))))))))))))

))))

(defmethod read-commits ((repo string) &rest args &key &allow-other-keys)
  (let* ((p (local-upload-pack repo)))
    (apply #'read-commits p args)))

;; (log:config :warn)
;; (read-commits "/home/arnold/builds/fast-example/.git" :branch "refs/heads/master")
;; (length (read-commits "git@github.com:tdrhq/fast-example.git" :branch "refs/heads/master" :haves (list "3c6fcd29ecdf37a2d1a36f46309787d32e11e69b")))
;; (length (read-commits "git@github.com:tdrhq/fast-example.git" :wants (list "master") :depth 30))
;; (length (read-commits "git@github.com:tdrhq/fast-example.git" :wants nil :depth 30))
;; (length (read-commits "git@github.com:tdrhq/braft.git" :branch "master" :parse-parents t))
;; (read-commits "ssh://git@phabricator.tdrhq.com:2222/source/web.git" :wants (list "master") :depth 2)



