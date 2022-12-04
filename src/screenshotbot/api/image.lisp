;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/api/image
  (:use #:cl
        #:alexandria
        #:screenshotbot/api/core
        #:screenshotbot/model/image
        #:screenshotbot/model/screenshot)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:util
                #:oid)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/user-api
                #:current-company)
  (:import-from #:screenshotbot/model/image
                #:find-image-by-oid
                #:image-blob
                #:update-image
                #:make-image)
  (:import-from #:util/digests
                #:md5-file)
  (:export
   #:verify-image
   #:with-raw-post-data-as-tmp-file))
(in-package :screenshotbot/api/image)

(defvar *bucket* "screenshotbot")

(defclass temporary-credential ()
  ((api-key :type string
            :initarg :api-key
            :reader api-key)
   (api-secret :type string
               :initarg :api-secret
               :reader api-secret)))

(defmethod print-object ((c temporary-credential) out)
  (format out "#<TEMPORARY-CREDENTIALS ~A ~A>" (api-key c)
          (api-secret c)))

(defun get-sts-token ()
  (multiple-value-bind (out err ret)
   (uiop:run-program (list "aws" "sts" "get-federation-token" "--duration-seconds" "900"
                           "--name" "silkwrmuploader"
                           "--policy" (uiop:read-file-string (path:catfile *root* "s3-policy.json")))
                     :output 'string)
    (declare (ignore err))
    (assert (eql 0 ret))
    (let* ((resp (json:decode-json-from-string out))
           (cred (assoc-value resp :*credentials)))
      (make-instance 'temporary-credential
                     :api-key (assoc-value cred :*access-key-id)
                     :api-secret (assoc-value cred :*secret-access-key)))))

(defclass upload-response (api-response)
  ((type :initform "image")
   (id :type string
       :initarg :id
       :reader upload-response-id)
   (upload-url :type (or null string)
               :initarg :upload-url
               :reader upload-response-upload-url)))

(defparameter *use-blob-store-p* t)

(defun prepare-single-upload (hash content-type)
  (declare (ignore content-type))
  (let* ((image (find-image (current-company) hash))
         (uploadp (or (not image)
                      (not (verified-p image))))
         (image
           (or
            image
            (make-image :hash hash
                        :company (current-company))))
         (upload-url (when uploadp
                       (cond
                         (*use-blob-store-p*
                          (hex:make-full-url
                           hunchentoot:*request*
                           'api-upload-image-blob
                           :oid (oid image)))
                         (t
                          (error "S3 Blob store no longer supported"))))))
    (make-instance 'upload-response
                   :id (oid image)
                   :upload-url upload-url)))

(defapi (prepare-upload-api :uri "/api/screenshot") (hash content-type hash-list)
  (cond
    (hash
     (prepare-single-upload hash nil))
    (hash-list
     (loop for hash in (json:decode-json-from-string hash-list)
           collect
           (cons hash (prepare-single-upload hash nil))))
    (t
     (error 'api-error
            "provide either hash or hash-list argument"))))

(defhandler (nil :uri "/api/prepare-upload" :method :post) (hash content-type)
  (prepare-upload-api :hash hash :content-type content-type))

;; I'm skeptical of the correctness of this, see D5631
(defun %with-raw-post-data-as-tmp-file (fn)
  (uiop:with-temporary-file (:stream s :pathname p :direction :output
                             :element-type 'flexi-streams:octet)
    (let* (;;(content-length (parse-integer (hunchentoot:header-in* :content-length)))
           (in-stream (hunchentoot:raw-post-data :force-binary t
                                                 :want-stream t)))
      (let ((buf (make-array 4096 :element-type '(unsigned-byte 8))))
        (loop for bytes = (read-sequence buf in-stream)
              while (> bytes 0)
              do (write-sequence buf s :end bytes))))
    (finish-output s)
    (funcall fn p)))

(defmacro with-raw-post-data-as-tmp-file ((tmpfile) &body body)
  `(%with-raw-post-data-as-tmp-file (lambda (,tmpfile) ,@body)))

(defhandler (api-upload-image-blob :uri "/api/image/blob" :method :put) (oid)
  (let ((image (find-image-by-oid oid)))
    (with-raw-post-data-as-tmp-file (p)
      (update-image image :pathname p)
      (verify-image image)
      "0")))

(defmethod verify-image ((image local-image))
  "local-images don't need verification"
  t)

(defmethod verify-image (image)
  (unless (screenshotbot/model/image:verified-p image)
    ;; check again! could've been verified by now
    (with-local-image (file image)
     (let ((etag
             (md5-file file)))
       (cond
         ((equalp etag (image-hash image))
          (with-transaction ()
            (setf (screenshotbot/model/image:verified-p image) t)))
         (t
          (error 'api-error
                  (format nil
                          "md5sum mismatch from what was uploaded for image: ~a vs ~a"
                          etag
                          (ironclad:byte-array-to-hex-string
                           (image-hash image))))))))))
