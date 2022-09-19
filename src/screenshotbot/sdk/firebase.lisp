(defpackage :screenshotbot/sdk/firebase
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:parse-firebase-output))
(in-package :screenshotbot/sdk/firebase)

(defclass firebase-output ()
  ((bucket
    :initarg :bucket
    :reader firebase-output-bucket)
   (location :initarg :location
             :reader firebase-output-location)))


(defun parse-firebase-output (output)
  (let ((lines (str:lines output)))
    (loop for line in lines
          if (str:containsp "Raw results will be stored in" line)
            return
            (multiple-value-bind (full parts)
                (cl-ppcre:scan-to-strings
                 "\\[https://console.developers.google.com/storage/browser/(.*)/(.*)/\\]"
                 line)
              (unless full
                (error "Could not parse the firebase output file! Please reach out to support@screenshotbot.io"))
              (make-instance 'firebase-output
                              :bucket (elt parts 0)
                              :location (elt parts 1))))))
