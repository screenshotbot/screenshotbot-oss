;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/recaptcha
  (:use #:cl)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:markup
                #:xml-tag-children)
  (:export
   #:recaptcha))
(in-package :util/recaptcha)

(named-readtables:in-readtable markup:syntax)

(defclass recaptcha ()
  ((site-key :initarg :site-key
          :initform nil
          :reader recaptcha-site-key
          :documentation "This token will be used on web pages, so will be public.")
   (api-key :initarg :api-key
            :initform nil
            :reader recaptcha-api-key
            :documentation "This is the Google Cloud API key, different from the token, and is meant to be private.")))

(defclass installation-with-recaptcha ()
  ((recaptcha :initarg :recaptcha
              :initform nil
              :reader recaptcha)))

(defmethod recaptcha (installation)
  nil)

(defmethod recaptcha-verify-token ((self recaptcha)
                                   (token string))
    (let ((body `((:event . ((:token . ,token)
                             (:expected-action . "signup")
                             (:site-key . ,(recaptcha-site-key self)))))))
      (multiple-value-bind (body res)
        (http-request
         (format nil "https://recaptchaenterprise.googleapis.com/v1/projects/screenshotbot/assessments?key=~a"
                 (recaptcha-api-key self))
         :content-type "application/json"
         :method :post
         :content (json:encode-json-to-string body))
      (unless (eql 200 res)
        (error "Recaptcha failed with: ~a" body))
      (let* ((obj (json:decode-json-from-string body))
             (score (assoc-value (assoc-value obj :risk-analysis) :score)))
        score))))

(defmethod recaptcha-verify-token ((self null) (token null))
  0.9)

(defmethod recaptcha-verify-token ((self recaptcha) (token null))
  "What if a bot tried to call the endpoint without the token?"
  0.001)

(defmethod recaptcha-annotate-form ((self null) form)
  form)

(auto-restart:with-auto-restart ()
  (defmethod recaptcha-annotate-form ((self recaptcha)
                                      form)
    (mquery:with-document (form)
      (let ((id (mquery:attr (mquery:$ "form") "id")))
        (assert (not (str:emptyp id)))
        (setf (xml-tag-children form)
              (list*
               <script>
               function onStandardSignupSubmit(token) {
               document.getElementById(",(progn id)").submit();
               }
               </script>
               (xml-tag-children form)))

        (let ((submit (mquery:$ "[type='submit']")))
          (assert submit)
          (setf (mquery:attr submit "type") nil)
          (setf (mquery:attr submit "data-sitekey")
                (recaptcha-site-key self))
          (setf (mquery:attr submit "data-callback")
                "onStandardSignupSubmit")
          (setf (mquery:attr submit "data-action")
                "submit")
          (mquery:add-class submit "g-recaptcha"))))))

(defmethod recaptcha-add-head-script ((self null) x)
  x)

(auto-restart:with-auto-restart ()
  (defmethod recaptcha-add-head-script ((self recaptcha) x)
    (mquery:with-document (x)
      (let ((head (mquery:$ "head")))
        (mquery:mqappend
         head
         <script
           src=(format nil "https://www.google.com/recaptcha/enterprise.js?render=~a" (recaptcha-site-key self))
           ></script>
)))))
