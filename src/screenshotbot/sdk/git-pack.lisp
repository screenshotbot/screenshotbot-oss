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
           :reader %stream)))

(defun local-upload-pack (repo)
  (make-instance 'upload-pack
                 :stream
                 (sys:open-pipe
                  (format nil "bash -c \"git upload-pack ~a\" 2>&1 | tee /tmp/foo.txt " repo)
                  ;;(list "/usr/bin/env" "git" "upload-pack" repo)
                                :direction :io
                                :element-type '(unsigned-byte 8))))

(defmethod close-upload-pack (self)
  (close (%stream self)))

(defun read-length (self)
  (let ((len (make-array 4)))
    (assert (= 4 (read-sequence len (%stream self))))
    (log:info "Got length: ~a" len)
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
  (read-protocol-line self)
  (loop for line = (read-protocol-line self)
        while line
        collect (str:split " " (str:trim line))))


(defmethod write-packet (self fmt &rest content)
  (log:info "Writing packet ~a" (apply #'format nil fmt content))
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


(defun simulate ()

  ;; https://github.com/git/git/blob/master/Documentation/gitprotocol-pack.adoc
  (setf *p* (local-upload-pack "/home/arnold/builds/fast-example/.git/"))
  (read-headers *p*)
  (want *p* "7f2ca96d151b02e5a4e966a00584d79c9ff439f6 filter")
  ;; This requires something like git config --global --add  uploadpack.allowFilter 1
  ;; Unclear if this is always available.
  ;; (write-packet *p* "filter object:type=commit")
  (write-flush *p*)
  (write-packet *p* "done")
  (finish-output (%stream *p*))
  (read-protocol-line *p*)

  ;; From here on the rest is the packfile 

  (close-upload-pack *p*))




