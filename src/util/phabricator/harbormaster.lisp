;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/phabricator/harbormaster
  (:use #:cl)
  (:import-from #:util/phabricator/conduit
                #:call-conduit
                #:search-conduit
                #:phab-instance)
  (:import-from #:alexandria
                #:assoc-value
                #:hash-table-keys)
  (:import-from #:local-time
                #:timestamp+
                #:timestamp-to-unix)
  (:export
   #:download-file
   #:upload-file
   #:create-artifact
   #:builds-for-diffs
   #:build
   #:build-p
   #:build-name
   #:build-status
   #:build-status-name
   #:build-failed-p
   #:build-passed-p
   #:build-waiting-p))
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
         (upload-file-chunked phab phid stream)
         phid)
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

;;; * Builds
;;;
;;; What Harbormaster ran for a revision. Buildables hang off diffs
;;; rather than revisions -- a revision has one buildable for every diff
;;; it has ever had -- so the way in is the diff the revision currently
;;; points at, and the way back out is the buildable's container, which
;;; is the revision.

(defstruct build
  name
  status                                ; the raw value, e.g. "failed"
  status-name)                          ; and what to call it, e.g. "Failed"

(defparameter *failed-build-statuses*
  '("failed" "aborted" "error" "deadlocked")
  "The build statuses that mean it isn't going to pass.")

(defun build-failed-p (build)
  (and (member (build-status build) *failed-build-statuses* :test #'equal) t))

(defun build-passed-p (build)
  (equal "passed" (build-status build)))

(defun build-waiting-p (build)
  "Whether BUILD hasn't finished: pending, building, paused, or a status
we've never heard of. An unknown status counts as unfinished rather than
as passed, which is the answer that can't mislead."
  (not (or (build-failed-p build) (build-passed-p build))))

(defun parse-build (fields)
  (let ((status (assoc-value fields :build-status)))
    (make-build :name (assoc-value fields :name)
                :status (assoc-value status :value)
                :status-name (assoc-value status :name))))

(defmethod builds-for-diffs ((phab phab-instance) diff-phids &key (limit 1000))
  "The builds Harbormaster ran for the diffs in DIFF-PHIDS.

Returns a hash table from revision PHID to a list of BUILDs, which is
two calls for the whole list rather than two per revision. A diff with no
buildable, or a buildable with no builds, simply isn't in the table."
  (let ((builds (make-hash-table :test #'equal)))
    (when diff-phids
      (let ((revision-of (make-hash-table :test #'equal)))
        (dolist (buildable (search-conduit
                            phab "harbormaster.buildable.search"
                            :limit limit
                            :constraints `(("objectPHIDs" . ,(coerce diff-phids 'vector)))))
          (setf (gethash (assoc-value buildable :phid) revision-of)
                (assoc-value (assoc-value buildable :fields) :container-+phid+)))
        (when (plusp (hash-table-count revision-of))
          (dolist (row (search-conduit
                        phab "harbormaster.build.search"
                        :limit limit
                        :constraints `(("buildables" . ,(coerce (hash-table-keys revision-of)
                                                                'vector)))))
            (let* ((fields (assoc-value row :fields))
                   (revision (gethash (assoc-value fields :buildable-+phid+) revision-of)))
              (when revision
                (push (parse-build fields) (gethash revision builds))))))))
    builds))
