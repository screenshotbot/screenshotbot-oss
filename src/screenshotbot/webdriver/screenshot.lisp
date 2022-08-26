;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/webdriver/screenshot
  (:use #:cl)
  (:import-from #:screenshotbot/webdriver/impl
                #:chrome
                #:firefox
                #:take-screenshot)
  (:import-from #:webdriver-client
                #:http-post-value
                #:*session*
                #:session-path
                #:screenshot
                #:http-get
                #:http-get-value)
  (:import-from #:auto-restart
                #:with-auto-restart)
  (:local-nicknames (#:a #:alexandria)
                    #-lispworks
                    (#:fli #:util/fake-fli))
  (:export
   #:full-page-screenshot))
(in-package :screenshotbot/webdriver/screenshot)

(defun decode-image (encoded-image output)
  (when output
   (with-open-file (out-stream output :direction :output :element-type 'flexi-streams:octet)
     (base64:base64-string-to-stream encoded-image
                                     :stream out-stream))))

(defun take-screenshot (output)
  (when output
   (ensure-directories-exist output))
  (let ((encoded-image (screenshot)))
    (when output
     (decode-image encoded-image output))))

(defmethod full-page-screenshot (driver file)
  (take-screenshot file))

(with-auto-restart (:retries 3 :sleep 3)
  (defmethod full-page-screenshot :around (driver file)
    (call-next-method)))

(defmethod full-page-screenshot ((driver firefox) file)
  (with-open-stream (stream (drakma:http-request
                             (format nil "~a"
                              (webdriver-client::make-uri
                               (session-path *session* "/moz/screenshot/full")))
                             :want-stream t))
    (flet ((decode-image (file)
             (decode-file-from-json-stream
              stream
              "value"
              :output file)))
     (cond
       ((or (null file) (string-equal "png" (pathname-type file)))
        (decode-image  file))
       (t
        (error "type ~a not supported" (pathname-type file)))))))

(defmethod execute-cdp-command ((driver chrome) command args)
  (http-post-value
   (session-path *session* "/goog/cdp/execute")
   :cmd command
   :params args))

#+nil
(defparameter *buf-size* 1024
  "A buffer size for base64 decoding")

#-lispworks
 (defun read-base64-stream-to-file (stream output)
  (with-open-file (output output
                          :direction :output
                          :element-type '(unsigned-byte 8))
    (let ((output
            #+sbcl
            (flexi-streams:make-flexi-stream output :external-format :latin-1)
            #-sbcl
            output))
     (read-base64-stream-to-stream
      stream output))))

#-lispworks
(defun read-base64-stream-to-stream (input output)
-  ;; Can we do this faster?
  (handler-case
      (progn
        (base64:base64-stream-to-stream
         input
         :stream output))
    (base64:bad-base64-character (e)
      (assert (eql (char-code #\") (base64::bad-base64-character-code e))))))


#+lispworks
(defun read-base64-stream-to-file (stream output)
  (with-open-file (output output :element-type '(unsigned-byte 8)
                                 :direction :output)
    (let ((evp-ctx (evp-encode-ctx-new)))
      (unwind-protect
           (let* ((len (* 64 100))
                  (inbuff (make-array len :element-type 'base-char :allocation :pinnable))
                  (outbuff (make-array len :element-type '(unsigned-byte 8) :allocation :pinnable)))
             (evp-decode-init evp-ctx)
             (loop for bytes = (read-sequence inbuff stream)
                   for pass from 0
                   for bytes-2 = (find-double-quote inbuff bytes)
                   while (> bytes-2 0)
                   do
                      (multiple-value-bind (result out-size)
                          (evp-decode-update
                           evp-ctx
                           outbuff
                           (length outbuff)
                           inbuff
                           bytes-2)
                        (assert (<= 0 result 1))
                        (write-sequence
                         outbuff
                         output
                         :end out-size)))
             (multiple-value-bind (result out-size)
                 (evp-decode-final
                  evp-ctx
                  outbuff
                  (length outbuff))
               (assert (= result 1))
               (write-sequence
                outbuff
                output
                :end out-size)))
        (evp-encode-ctx-free evp-ctx)))))

(defun find-double-quote (arr len)
  (declare (type (array base-char) arr)
           (type fixnum len)
           (optimize (speed 3) (debug 0)))
  (loop for i fixnum from 0 below len
        if (eql (aref arr i) #\")
          do (return-from find-double-quote i)
        finally (return len)))

(fli:define-c-struct evp-encode-ctx)

(fli:define-foreign-function (evp-encode-ctx-new "EVP_ENCODE_CTX_new")
    ()
  :result-type (:pointer evp-encode-ctx))

(fli:define-foreign-function (evp-encode-ctx-free "EVP_ENCODE_CTX_free")
    ((ctx (:pointer evp-encode-ctx)))
  :result-type :void)

(fli:define-foreign-function (evp-decode-init "EVP_DecodeInit")
    ((ctx (:pointer evp-encode-ctx)))
  :result-type :void)

#+lispworks
(fli:define-foreign-function (evp-decode-update "EVP_DecodeUpdate")
    ((ctx (:pointer evp-encode-ctx))
     (out :lisp-simple-1d-array)
     (out-size (:reference-return :int))
     (in :lisp-simple-1d-array)
     (in-size :int))
  :result-type :int)

#+lispworks
(fli:define-foreign-function (evp-decode-final "EVP_DecodeFinal")
    ((ctx (:pointer evp-encode-ctx))
     (out :lisp-simple-1d-array)
     (out-size (:reference-return :int)))
  :result-type :int)

(defun decode-file-from-json-stream (stream key &key (output (error "must provide :output")))
  "Given a json, with a value that is a large base64 encoded file, decode the base64 file and save it "
  (let* ((found-data-p nil)
         (decoder (cl-json:custom-decoder
                   :beginning-of-string
                   (lambda ()
                     (cond
                       (found-data-p
                        ;; We're going to treat the rest of the
                        ;; stream as just a Base64 stream.
                        (when output
                          (read-base64-stream-to-file stream output))
                        (return-from decode-file-from-json-stream nil))
                       (t
                        (json::init-string-stream-accumulator))))
                   :end-of-string
                   (lambda ()
                     (let ((ret (json::string-stream-accumulator-get)))
                       (when (equal key ret)
                         (setf found-data-p t))
                       ret)))))
    (funcall decoder stream)
    ;; The decode shoul
    d return from the toplevel function
    (error "Did not find a :data attribute in the response json")))


(defmethod execute-streaming-cdp-command ((driver chrome) command args &key output)
  (with-open-stream (stream (drakma:http-request
                             (format nil "~a"
                                     (webdriver-client::make-uri (session-path webdriver-client::*session* "/goog/cdp/execute")))
                             :content (cl-json:encode-json-plist-to-string
                                       `(:cmd ,command :params ,args))
                             :content-type "application/json:charset=UTF-8"
                             :method :post
                             :want-stream t))
    (decode-file-from-json-stream
     stream
     "data"
     :output output)))

(defmethod full-page-screenshot ((driver chrome) file)
  ;; This is documented in some places, for example:
  ;; https://stackoverflow.com/questions/41721734/take-screenshot-of-full-page-with-selenium-python-with-chromedriver
  ;; ... but as of now, not in the official selenium code
  (let ((rect (execute-cdp-command driver "Page.getLayoutMetrics" (make-hash-table))))

    ;; Let's try to limit the screenshot to 25 million pixels (5000x5000).
    (let* ((max-pixels 12500000)
           (additional-scale-factor 1)
           (visual-viewport (a:assoc-value rect :css-visual-viewport))
           (zoom (a:assoc-value visual-viewport :zoom))
           (pixel-ratio (webdriver-client:execute-script "return window.devicePixelRatio" nil))
           (content-size (a:assoc-value rect :content-size))
           (width (a:assoc-value content-size :width))
           (height (min
                    (a:assoc-value content-size :height)
                    (floor max-pixels (max 1 width)))))


      (assert (= zoom 1))
      #+nil
      (assert (= pixel-ratio 1))


      (let ((screenshot (execute-streaming-cdp-command driver "Page.captureScreenshot"
                                                       `(("captureBeyondViewport" . t)
                                                         ("clip" . ((:x . 0)
                                                                    (:y . 0)
                                                                    (:height . ,height)
                                                                    (:width . ,width)
                                                                    (:scale . ,(/ 1.0 pixel-ratio
                                                                                  additional-scale-factor))))
                                                         (:format . "png"))
                                                       :output file)))
        (when file
         (assert (< (trivial-file-size:file-size-in-octets file)
                    25000000)))))))
