;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/phabricator/harbormaster
  (:use #:cl)
  (:import-from #:util/phabricator/conduit
                #:call-conduit
                #:phab-instance)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:local-time
                #:timestamp+
                #:timestamp-to-unix)
  (:export
   #:download-file
   #:upload-file
   #:create-artifact))
(in-package :util/phabricator/harbormaster)

(defun delete-after-epoch ()
  (timestamp-to-unix
   (timestamp+
    (local-time:now)
    1 :day)))

(defun p (x)
  #+nil
  (log:info "result: ~s" x)
  x)

(defmethod file-allocate ((phab phab-instance) file-size &key (name (Error "needs name")))
  (log:info "allocating file")
  (assoc-value
   (assoc-value
    (call-conduit
     phab
     "file.allocate"
     `(("name" . ,name)
       ("contentLength" . ,(p file-size))
       ("deleteAfterEpoch" . ,(delete-after-epoch))))
    :result)
   :file-+phid+))

(defmethod upload-file ((phab phab-instance) pathname
                        &key (name "unnamed"))
  "Upload a file and return the PHID"

  (with-open-file (stream pathname :direction :input
                                   :element-type 'flex:octet)
    (let ((phid (file-allocate phab (file-length stream) :name name)))
      (cond
        (phid
         (upload-file-chunked phab phid stream))
        (t
         (let ((arr (make-array (file-length stream)
                                :element-type 'flex:octet)))
           (read-sequence arr stream)
           (let ((response
                   (call-conduit
                    phab
                    "file.upload"
                    `(("name" . ,name)
                      ("data_base64" . ,(base64:usb8-array-to-base64-string arr))))))
             (assoc-value response :result))))))))

(defmethod upload-file-chunked ((phab phab-instance)
                                phid
                                stream)
  (let ((chunks (reverse (file-query-chunks phab phid))))
    (let ((buf (make-array 1 :element-type '(unsigned-byte 8)
                               :adjustable t
                               :fill-pointer t)))
     (dolist (chunk chunks)
       (assert (not (assoc-value chunk :complete)))
       (let* ((start (parse-integer (assoc-value chunk :byte-start)))
              (end (parse-integer (assoc-value chunk :byte-end))))
         (adjust-array buf (- end start)
                       :fill-pointer (-  end start))
         (file-position stream start)
         (read-sequence buf stream)
         (file-upload-chunk phab phid start buf))))))

(defmethod file-query-chunks ((phab phab-instance)
                              phid)
  (assoc-value
   (call-conduit
    phab
    "file.querychunks"
    `(("filePHID" . ,phid)))
   :result))

(defmethod file-upload-chunk ((phab phab-instance)
                              phid
                              pos
                              seq)
  (log:info "Uploading chunk at pos ~a" pos)
  (call-conduit
   phab
   "file.uploadchunk"
   `(("filePHID" . ,phid)
     ("byteStart" . ,pos)
     ("data" . ,(base64:usb8-array-to-base64-string seq))
     ("dataEncoding" . "base64"))))

(defmethod download-file ((phab phab-instance) phid output)
  (let ((response
          (call-conduit
           phab
           "file.download"
           `(("phid" . ,phid)))))
    (assert (not (assoc-value response :error--code)))
    (let ((base64 (assoc-value response :result)))
      (with-open-file (stream output :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
        (base64:base64-string-to-stream base64
                                        :stream
                                        #-sbcl
                                        stream
                                        #+sbcl
                                        (flex:make-flexi-stream
                                         stream
                                         :external-format :latin-1))))))

(defmethod create-artifact ((phab phab-instance)
                            phid
                            file
                            &key (name (error "must provide artifact name")))
  (let ((file-phid (upload-file phab file)))
    (let ((data (make-hash-table :test #'equal)))
      (setf (gethash "filePHID" data) file-phid)
      (call-conduit
       phab
       "harbormaster.createartifact"
       `(("buildTargetPHID" . ,phid)
         ("artifactKey" . ,name)
         ("artifactType" . "file")
         ("artifactData" . ,data))))))
