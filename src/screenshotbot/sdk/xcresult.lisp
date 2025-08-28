(defpackage :screenshotbot/sdk/xcresult
  (:use :cl))
(in-package :screenshotbot/sdk/xcresult)

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



;; Example usage:
;; (export-all-attachments #P"~/Downloads/results.xcresult" #P"/tmp/attachments/")

