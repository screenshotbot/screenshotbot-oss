;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/slack/core
    (:use #:cl
          #:alexandria
          #:util/java
          #:../user-api
          #:../model/company)
  (:import-from #:./plugin
                #:slack-plugin
                #:client-id
                #:client-secret)
  (:import-from #:../installation
                #:with-plugin)
  (:import-from #:bknr.datastore
                #:store-object
                #:hash-index
                #:with-transaction
                #:persistent-class)
  (:import-from #:../server
                #:defhandler)
  (:import-from #:../secret
                #:secret)
  (:export #:slack-token
           #:latest-slack-token
           #:slack-error-code
           #:slack-message-failed
           #:slack-post-on-channel
           #:slack-methods)
  ;; forward decl
  (:export #:find-or-create-slack-config))

(named-readtables:in-readtable java-syntax)


(defclass slack-token (store-object)
  ((access-token
    :initarg :access-token
    :initform nil
    :reader access-token)
   (company
    :initarg :company
    :accessor slack-token-company
    :index-type hash-index
    :index-initargs (:test 'eql)
    :index-reader slack-tokens-for-company)
   (ts
    :initform (get-universal-time)
    :initarg :ts
    :reader %created-at))
  (:metaclass persistent-class))

(defun latest-slack-token (company)
  (let ((token
         (car (sort (slack-tokens-for-company company)
                    #'> :key '%created-at))))
    (when token
     (values (access-token token) token))))

(define-condition slack-message-failed (error)
  ((error-code
    :initarg :error-code
    :accessor slack-error-code)))

(defun slack-instance ()
  (#_getInstance #,com.slack.api.Slack))

(defun slack-methods (&key token)
  (check-type token string)
  (#_methods (slack-instance) token))

(defun check-slack-ok (response)
  (unless (equal "true" (#_toString (#_isOk response)))
    (error "Slack API error: ~a" (#_getError response))))

(defun slack-post-on-channel (&key channel text methods)
  (let ((builder (#_builder #,com.slack.api.methods.request.chat.ChatPostMessageRequest)))
    (#_channel builder channel)
    (#_text builder text)
    (let ((response (#_chatPostMessage methods (#_build builder))))
      (check-slack-ok response)
      t)))

(defhandler (nil :uri "/slack-app-redirect") (code state)
  (declare (ignore state))
  (with-plugin (slack-plugin)
   (cond
     ((null code)
      (hex:safe-redirect "/settings/slack"))
     (t
      (let ((builder (#_builder #,com.slack.api.methods.request.oauth.OAuthV2AccessRequest)))
        (#_code builder code)
        (#_clientId builder (client-id slack-plugin))
        (#_clientSecret builder (client-secret slack-plugin))
        (#_redirectUri builder
                       (hex:make-full-url
                        hunchentoot:*request*
                        "/slack-app-redirect"))
        (let* ((methods (#_methods (slack-instance)  (client-id slack-plugin)))
               (response (#_oauthV2Access methods (#_build builder))))
          (check-slack-ok response)
          (let ((access-token (#_getAccessToken response)))
            (let ((token (make-instance 'slack-token
                                         :access-token access-token
                                         :company (current-company))))
              (with-transaction ()
                (setf (access-token (find-or-create-slack-config (current-company)))
                      token)))

            (hex:safe-redirect "/settings/slack"))))))))

;; (slack-post-on-channel :channel "#random" :text "foobar2"
;;                        :methods (slack-methods :token "xoxb-1422927031269-1607469573089-WIpznshstwSK3yBTtnDhj4de"))
