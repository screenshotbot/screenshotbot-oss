(defpackage :screenshotbot/sdk/xcresult
  (:use :cl)
  (:import-from #:screenshotbot/sdk/bundle
                #:image-name
                #:list-images)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:iterate
                #:iter))
(in-package :screenshotbot/sdk/xcresult)

(defclass xcresults-attachment-bundle ()
  ((directory :initarg :directory
              :reader %directory)))

(defclass xcresults-attachment ()
  ((bundle :initarg :bundle
           :reader bundle)
   (name :initarg :name
         :reader image-name)
   (file-name :initarg :file-name
              :reader file-name)))

(defun export-all-attachments (xcresult-path output-path)
  "Export all attachments from the xcresult file to the given output directory.
   Creates a manifest.json and all attachment files (PNGs, logs, etc.)."
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (list "xcrun" "xcresulttool" "export" "attachments"
                              "--path" (namestring xcresult-path)
                              "--output-path" (namestring output-path))
                        :output :string 
                        :error-output :string 
                        :ignore-error-status t)
    (if (zerop exit-code)
        (values t output)  ; Return success and any output message
        (error "Failed to export attachments: ~A" error-output))))

(defun parse-name (suggested-name test-identifier)
  (let ((parts (reverse (str:split "/" test-identifier))))
    (str:join "/"
     (reverse
      (list*
       (cl-ppcre:regex-replace-all "^SnapshotTest_"
                                   (cl-ppcre:regex-replace-all "_[0-9A-Z-]*.png$" suggested-name "")
                                   "")
       (cdr parts))))))

(defmethod list-images ((self xcresults-attachment-bundle))
  (let ((manifest (json:decode-json-from-string
                   (uiop:read-file-string (path:catfile (%directory self) "manifest.json")))))
    (loop for entry in manifest
          for test-identifier = (assoc-value entry :test-identifier)
          appending
          (loop for attachment in (assoc-value entry :attachments)
                for suggested-name = (assoc-value attachment :suggested-human-readable-name)
                for exported-file-name = (assoc-value attachment :exported-file-name)
                if (and
                    (str:starts-with-p "SnapshotTest_" suggested-name)
                    (str:ends-with-p ".png" suggested-name))
                collect
                (make-instance 'xcresults-attachment
                               :bundle self
                               :name (parse-name suggested-name test-identifier)
                               :file-name exported-file-name)))))



;; Example usage:
;; (export-all-attachments #P"~/Downloads/results.xcresult" #P"/tmp/attachments/")

