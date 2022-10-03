(defpackage :screenshotbot/sdk/firebase
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:local-nicknames (#:a #:alexandria)
                    (#:flags #:screenshotbot/sdk/flags))
  (:export
   #:parse-firebase-output
   #:with-firebase-output))
(in-package :screenshotbot/sdk/firebase)

(defclass firebase-output ()
  ((bucket
    :initarg :bucket
    :reader firebase-output-bucket)
   (location :initarg :location
             :reader firebase-output-location)))


(defun parse-firebase-output (output)
  (flet ((oops ()
           (error "Could not parse the firebase output file! Please reach out to support@screenshotbot.io")))
   (let ((lines (str:lines output)))
     (loop for line in lines
           if (str:containsp "Raw results will be stored in" line)
             return
             (multiple-value-bind (full parts)
                 (cl-ppcre:scan-to-strings
                  "\\[https://console.developers.google.com/storage/browser/(.*)/(.*)/\\]"
                  line)
               (unless full
                 (oops))
               (make-instance 'firebase-output
                               :bucket (elt parts 0)
                               :location (elt parts 1)))
           finally
              (oops)))))

(defun find-file (dir file-name)
  (cond
    ((and
      (equal (pathname-name file-name) (pathname-name dir))
      (equal (pathname-type file-name) (pathname-type dir)))
     dir)
    (t
     (loop for child in (fad:list-directory dir)
           for artifact = (find-file child file-name)
           if artifact
             return artifact))))


(def-easy-macro with-firebase-output (filename &fn fn)
  (let ((firebase-output (parse-firebase-output (uiop:read-file-string filename))))
    (tmpdir:with-tmpdir (dir)
      (let ((cloud-location (format nil "gs://~a/~a/"
                                    (firebase-output-bucket firebase-output)
                                    (firebase-output-location firebase-output))))
        (log:info "Downloading screenshots from Google Cloud: ~a" cloud-location)
        (uiop:run-program
         (list "gcloud" "alpha" "storage" "cp" "-r"
               cloud-location
               (namestring dir))
         :output *standard-output*
         :error-output *standard-output*)
        (log:info "Cleaning up the Google cloud directory before we continue")
        #+nil
        (uiop:run-program
         (list "gcloud" "alpha" "storage" "rm" "-r"
               cloud-location)
         :output *standard-output*
         :error-output *standard-output*))

      (log:info "Downloaded screenshots, processing it locally")
      (let ((flags:*metadata*
              (remove-if #'null
               (list
                (find-file dir "metadata.json")
                (find-file dir "metadata_compose.json")))))
        (funcall fn)))))
