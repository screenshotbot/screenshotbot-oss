;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/git-pack
  (:use #:cl))
(in-package :screenshotbot/sdk/git-pack)

(defclass upload-pack ()
  ((stream :initarg :stream
           :accessor %stream)))

(defun make-upload-pack-command (repo)
  (multiple-value-bind (match parts)
      (cl-ppcre:scan-to-strings "^(ssh://)?([a-zA-Z0-9]*)@([a-zA-Z0-9.]*):(.*)$"
                                repo)
   (cond
     (match
      (format nil "ssh ~a@~a git upload-pack ~a 2>/dev/null"
              (elt parts 1)
              (elt parts 2)
              (elt parts 3)))
     (t
      "git upload-pack ~a 2>/dev/null"))))

(defun local-upload-pack (repo)
  (make-instance 'upload-pack
                 :stream
                 (sys:open-pipe
                  (format nil (make-upload-pack-command repo))
                  ;;(list "/usr/bin/env" "git" "upload-pack" repo)
                                :direction :io
                                :element-type '(unsigned-byte 8))))

(defmethod close-upload-pack (self)
  (close (%stream self)))

(defun read-length (self)
  (let ((len (make-array 4)))
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
       (let ((content (make-array (- len 4))))
         (read-sequence content (%stream self))
         (prog1
             (flex:octets-to-string content)))))))

(defmethod read-headers (self)
  ;;  () - Got line:829e9b346306604777c9dcc2c09b61aa30511446 HEAD ... multi_ack thin-pack side-band side-band-64k ofs-delta shallow deepen-since deepen-not deepen-relative no-progress include-tag multi_ack_detailed symref=HEAD:refs/heads/new-pr filter object-format=sha1 agent=git/2.39.5
  ;; In particular we care about the filter capability in the future.

  (read-protocol-line self)
  (loop for line = (read-protocol-line self)
        while line
        collect (str:split " " (str:trim line))))


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
  (let ((magick (make-array 4)))
    (read-sequence magick stream)
    (assert (equal "PACK" (flex:octets-to-string magick))))

  (let ((version (make-array 4)))
    (read-sequence version stream)
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


(defun read-packfile-entry (packfile)
  "Returns type and the contents of the entry"
  ;; https://github.com/git/git/blob/master/Documentation/gitprotocol-pack.adoc
  (log:trace "Reading packfile entry")
  (let ((stream (%stream packfile)))
    (multiple-value-bind (type length) (read-entry-header stream)
      (log:trace "Got type: ~a" type)
      (assert (not (member type '( 6 )))) ;; not supported yet

      (when (eql 7 type #| OBJ_REF_DELTA |#)
        (let ((arr (make-array 20)))
          ;; Read the name of the base object
          (read-sequence arr stream)))

      (let ((body (flex:make-in-memory-output-stream))
            (dstate (chipz:make-dstate :zlib)))
        (chipz:decompress
         body
         dstate
         stream)

        (let ((body (flex:get-output-stream-sequence body)))
          (setf
           (%stream packfile)
           ;; TODO: we could potentially optimize this.. but do we care?
           (safe-make-concatenated-stream
            (flex:make-in-memory-input-stream
             (chipz::inflate-state-input dstate)
             :start (chipz::inflate-state-input-index dstate)
             :end (chipz::inflate-state-input-end dstate))
            (%stream packfile)))
          (assert (= length (length body)))
          (values type
                  body
                  length
                  dstate))))))

(defun packfile-test ()
  (simulate)
  (setf *arr* (make-array 4))
  (let ((num (read-packfile-header (%stream *p*))))
    (loop for i below num
          collect
             (multiple-value-list
              (read-packfile-entry *p*))))

  
  (close-upload-pack *p*))

(defun read-commits (repo &key branch)
  (let* ((p (local-upload-pack repo))
         (headers (read-headers p)))
    (let ((wants
            (loop for this-branch in headers
                  if (equal branch (second this-branch))
                    collect (first this-branch))))
      (unless wants
        (warn "Could not find branch in ~a" headers))
      (when wants
        (want p (format nil "~a filter" (car wants))))
      (dolist (want (cdr wants))
        (want p want))
      (write-flush p)
      (write-packet p "done")
      (finish-output (%stream p))

      (read-protocol-line p)

      ;; Now we get the packfile
      (let ((num (read-packfile-header (%stream p))))
        (serapeum:collecting
          (loop for i below num
                do
                   (multiple-value-bind (type body) (read-packfile-entry p)
                     (when (eql 1 type)
                       (collect (flex:octets-to-string body))))))))))

;; (log:config :warn)
;; (read-commits "/home/arnold/builds/fast-example/.git" :branch "refs/heads/master")
;; (read-commits "git@github.com:tdrhq/fast-example.git" :branch "refs/heads/master")
;; (read-commits "git@github.com:tdrhq/braft.git" :branch "refs/heads/master")




