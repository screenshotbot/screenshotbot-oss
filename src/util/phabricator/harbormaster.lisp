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
  (:export
   #:download-file
   #:upload-file))
(in-package :util/phabricator/harbormaster)

(defmethod upload-file ((phab phab-instance) pathname
                        &key (name "unnamed"))
  "Upload a file and return the PHID"

  (with-open-file (stream pathname :direction :input
                                   :element-type 'flex:octet)
    (let ((arr (make-array (file-length stream)
                           :element-type 'flex:octet)))
      (read-sequence arr stream)
      (let ((response
             (call-conduit
              phab
              "file.upload"
              `(("name" . ,name)
                ("data_base64" . ,(base64:usb8-array-to-base64-string arr))))))
        (assoc-value response :result)))))

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
