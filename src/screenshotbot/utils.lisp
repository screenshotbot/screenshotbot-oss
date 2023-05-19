(defpackage :screenshotbot-utils
  (:nicknames :sb-util)
  (:use #:cl
        #:alexandria)
  (:import-from #:screenshotbot/secret
                #:secret)
  (:import-from #:util/digests
                #:md5-file)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:export #:upload-fasl
           #:upload-sdk
           #:deploy-fasls-main
           #:load-tmp-fasl))
(in-package :screenshotbot-utils)

(defun md5-hex (f)
  #+lispworks
  (comm:ensure-ssl)
  (ironclad:byte-array-to-hex-string (md5-file f)))

(def-easy-macro with-compression (&binding filename filename &key compress &fn fn)
  (cond
    (compress
     (uiop:with-temporary-file (:type "gz" :pathname p)
       (gzip-stream:gzip filename p)
       (fn p)))
    (t
     (fn filename))))

(defun upload-artifact (name filename &key compress)
  (log:info "Uploading via asset")
  (let ((hash (md5-hex filename)))
   (with-compression (filename filename :compress compress)
     (multiple-value-bind (result code)
         (drakma:http-request "https://screenshotbot.io/intern/artifact/upload"
                              :method :put
                              :force-binary t
                              :write-timeout 75
                              :parameters `(("name" . ,name)
                                            ("hash" . ,hash)
                                            ("upload-key" . ,(secret :artifact-upload-key))
                                            ("compressed" . ,(if compress "true")))
                              :content (pathname filename))
       (log:info "Got image upload response: ~s" (flexi-streams:octets-to-string result))
       (unless (eql 200 code)
         (error "Failed to upload image: code ~a" code)))))
  (log:info "Upload done"))

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
                      #+(or win32 mswindows) "recorder-win.exe"
                      output-file
                      #+mswindows :compress #+mswindows t))))

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
