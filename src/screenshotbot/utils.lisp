(defpackage :screenshotbot-utils
  (:nicknames :sb-util)
  (:use #:cl
        #:alexandria)
  (:import-from #:screenshotbot/secret
                #:secret)
  (:import-from #:util/digests
                #:md5-file)
  (:export #:upload-fasl
           #:upload-sdk
           #:deploy-fasls-main
           #:load-tmp-fasl))
(in-package :screenshotbot-utils)

(defun md5-hex (f)
  (ironclad:byte-array-to-hex-string (md5-file f)))

(defun upload-artifact (name filename)
  (log:info "Uploading via scp")
  (uiop:run-program (list
                     "scp"
                     (namestring filename)
                     "web@screenshotbot.io:~/web/tmp-upload")
                    :output :interactive
                    :input :interactive
                    :error-output :interactive)
  (log:info "Upload done")
  (multiple-value-bind (result code)
      (drakma:http-request "https://screenshotbot.io/intern/artifact/upload"
                           :method :put
                           :force-binary t
                           :parameters `(("name" . ,name)
                                         ("hash" . ,(md5-hex filename))
                                         ("upload-key" . ,(secret :artifact-upload-key))))
    (log:info "Got image upload response: ~s" (flexi-streams:octets-to-string result))
    (unless (eql 200 code)
      (error "Failed to upload image: code ~a" code))))

(defun upload-sdk (&key only-build)
  (asdf:compile-system :screenshotbot.sdk/deliver)
  #+nil
  (asdf:compile-system :screenshotbot.sdk/deliver/java-so)
  (let ((output-file (asdf:output-file 'asdf:compile-op
                                        (asdf:find-component
                                        :screenshotbot.sdk/deliver
                                        #-(or mswindows win32) "installer"
                                        #+(or mswindows win32) "deliver-sdk"))))
    (log:info "Output file is: ~a" output-file)
    (assert (path:-e output-file))
    (unless only-build
     (upload-artifact #+darwin "recorder-darwin"
                      #+linux "recorder-linux"
                      #+(or win32 mswindows) "recorder-win"
                      output-file))))

(defun upload-fasl (op system)
  (let ((op (or op 'asdf:compile-bundle-op)))
    (asdf:compile-system system)
    (asdf:perform op system)
    (let ((output (asdf:output-file op system)))
      (uiop:run-program (list
                         "rsync" "-z" (namestring output)
                         "web@screenshotbot.io:~/tmp.64ufasl")
                        :output :interactive
                        :error-output :interactive))))

;; (upload-fasl 'asdf:compile-bundle-op :screenshotbot)

(defun load-tmp-fasl ()
  (load "../tmp.64ufasl"))


(defun deploy-fasls-main (system)
  (log:info "Uploading system: ~A" system)
  (upload-fasl nil system))
