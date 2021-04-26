;;unused

(defpackage :grubhub
  (:use :cl
   :alexandria))
(in-package :grubhub)


(defclass session ()
  ((cookie-jar :initform (make-instance 'drakma:cookie-jar))))

(defun login (login password)
  (let ((session (make-instance 'session)))
    (with-open-stream
        (s
         (drakma:http-request
          "https://api-gtm.grubhub.com/auth"
          :method :post
          :content-type "application/json;charset=UTF-8"
          :additional-headers
          `(("Authorization" . "Bearer 81d44c4e-8ece-4ec5-be14-a6892cd4b5d9")
            ("x-unused" . "0")
            ("Origin" . "https://www.seamless.com")
            ("gh-request-id" . "2bcb9e10-14db-11eb-84e7-c5b122fa6858")
            ("TE" . "Trailers"))
          :content (json:encode-json-to-string
                    `((:brand . "SEAMLESS")
                      ("client_id" . "beta_seamless_ayNyuFxxVQYefSAhFYCryvXBPQc")
                      ("device_id" . 1140476417)
                      ("email" . ,login)
                      ("password" . ,password)))
          :want-stream t))
      (json:decode-json s))))

;;(login "arnold@tdrhq.com" "de1dr@n0r")
