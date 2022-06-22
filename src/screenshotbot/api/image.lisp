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
                #:find-by-oid
                #:oid)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/user-api
                #:current-company)
  (:import-from #:screenshotbot/model/image
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
       :initarg :id)
   (upload-url :type (or null string)
               :initarg :upload-url)))

#+lispworks
(lw-ji:define-java-callers     "io.tdrhq.TdrhqS3"
  (build-presigned-put
   "getPresignedPut")
  (%get-etag "getEtag"))

#-lispworks
(defun build-presigned-put (bucket key)
  (declare (ignore bucket key))
  (error "build-presigned-put: unsupported on non-lispworks"))

#-lispworks
(defun %get-etag (&rest args)
  (error "%get-etag: unsupported on non-lispworks"))

(defvar *build-presigned-put* 'build-presigned-put)

(defparameter *use-blob-store-p* t)

(defun prepare-single-upload (hash content-type)
  (let* ((image (find-image (current-company) hash))
         (uploadp (not image))
         (image
           (or
            image
            (make-image :hash hash
                        :company (current-company)
                        :content-type content-type)))
         (upload-url (when uploadp
                       (cond
                         (*use-blob-store-p*
                          (hex:make-full-url
                           hunchentoot:*request*
                           'api-upload-image-blob
                           :oid (oid image)))
                         (t
                          (let ((blob (make-instance 's3-blob)))
                            (with-transaction ()
                              (setf (image-blob image)
                                    blob)))
                          (funcall *build-presigned-put*
                                   *bucket*
                                   (s3-key image)))))))
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

(defhandler (nil :uri "/api/prepare-upload" :method :post :html nil) (hash content-type)
  (prepare-upload-api :hash hash :content-type content-type))


(defun %with-raw-post-data-as-tmp-file (fn)
  (uiop:with-temporary-file (:stream s :pathname p :direction :output
                             :element-type 'flexi-streams:octet)
    (let* (;;(content-length (parse-integer (hunchentoot:header-in* :content-length)))
           (in-stream (hunchentoot:raw-post-data :force-binary t
                                                 :want-stream t)))
      (fad:copy-stream in-stream (flexi-streams:make-flexi-stream s)))
    (finish-output s)
    (funcall fn p)))

(defmacro with-raw-post-data-as-tmp-file ((tmpfile) &body body)
  `(%with-raw-post-data-as-tmp-file (lambda (,tmpfile) ,@body)))

(defhandler (api-upload-image-blob :uri "/api/image/blob" :method :put) (oid)
  (let ((image (find-by-oid oid)))
    (with-raw-post-data-as-tmp-file (p)
      (with-transaction ()
        (setf (image-blob image)
              (bknr.datastore:make-blob-from-file
               p 'image-blob :type :png)))
      (verify-image image)
      "0")))

(defun get-etag (bucket key)
  (restart-case
      (%get-etag bucket key)
    (retry ()
      (get-etag bucket key))))

(defmethod verify-image ((image local-image))
  "local-images don't need verification"
  t)

(defmethod verify-image (image)
  (unless (screenshotbot/model/image:verified-p image)
    ;; check again! could've been verified by now
    (let ((etag
            (cond
              ((image-blob image)
               (md5-file (bknr.datastore:blob-pathname  (image-blob image))))
              (t
               (ironclad:hex-string-to-byte-array
                (get-etag *bucket* (s3-key image)))))))
      (cond
        ((equalp etag (image-hash image))
         (with-transaction ()
          (setf (screenshotbot/model/image:verified-p image) t)))
        (T
         (error 'api-error
                 (format nil
                         "md5sum mismatch from what was uploaded for image: ~a vs ~a"
                         etag
                         (ironclad:byte-array-to-hex-string
                          (image-hash image)))))))))
