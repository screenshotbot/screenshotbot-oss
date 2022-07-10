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
  (:import-from #:screenshotbot/magick
                #:magick
                #:convert-to-lossless-webp
                #:run-magick)
  (:local-nicknames (#:a #:alexandria))
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
        (uiop:with-temporary-file (:pathname png :type "png" :prefix "full-page-screenshot")
          (delete-file png)
          (decode-image png)
          (log:info "Converting firefox png to webp")
          (convert-to-lossless-webp
           (magick)
           png file)))))))

(defmethod execute-cdp-command ((driver chrome) command args)
  (http-post-value
   (session-path *session* "/goog/cdp/execute")
   :cmd command
   :params args))

#+nil
(defparameter *buf-size* 1024
  "A buffer size for base64 decoding")

(defun read-base64-stream-to-file (stream output)
  (with-open-file (output output
                          :direction :output
                          :element-type '(unsigned-byte 8))
    (read-base64-stream-to-stream
     stream output)))

(defun read-base64-stream-to-stream (input output)
  ;; Can we do this faster?
  (handler-case
      (progn
        (base64:base64-stream-to-stream
         input
         :stream output))
    (base64:bad-base64-character (e)
      (assert (eql (char-code #\") (base64::bad-base64-character-code e))))))

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
    ;; The decode should return from the toplevel function
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
