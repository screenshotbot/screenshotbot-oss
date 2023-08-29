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


(defun load-tmp-fasl ()
  (load "../tmp.64ufasl"))


(defun deploy-fasls-main (system)
  (log:info "Uploading system: ~A" system)
  (upload-fasl nil system))
