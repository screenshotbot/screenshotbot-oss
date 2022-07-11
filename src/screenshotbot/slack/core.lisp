;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/slack/core
  (:use #:cl
        #:alexandria
        #:util/java
        #:screenshotbot/user-api
        #:screenshotbot/model/company)
  (:import-from #:screenshotbot/slack/plugin
                #:slack-plugin
                #:client-id
                #:client-secret)
  (:import-from #:screenshotbot/installation
                #:with-plugin)
  (:import-from #:bknr.datastore
                #:store-object
                #:hash-index
                #:with-transaction
                #:persistent-class)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/secret
                #:secret)
  (:export
   #:slack-token
   #:latest-slack-token
   #:slack-error-code
   #:slack-message-failed
   #:slack-post-on-channel
   #:slack-methods
   #:audit-log
   #:slack-audit-logs-for-company
   #:post-on-channel-audit-log)
  ;; forward decls
  (:export #:find-or-create-slack-config))
(in-package :screenshotbot/slack/core)

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

(defclass audit-log (store-object)
  ((company :initarg :company
            :index-type hash-index
            :index-reader %slack-audit-logs-for-company)
   (err :initarg :error
        :initform nil
        :accessor audit-log-error))
  (:metaclass persistent-class))

(defun slack-audit-logs-for-company (company)
  (let ((logs (%slack-audit-logs-for-company company)))
    ;; the BKNR datastore indices is super buggy for this
    ;; index. Probably because of the inheritence.
    (let ((hash-table (make-hash-table)))
      (loop for log in logs
            unless (bknr.datastore::object-destroyed-p log)
            do (setf (gethash log hash-table) t))
      (sort (alexandria:hash-table-keys hash-table)
            #'> :key #'bknr.datastore:store-object-id))))

(defclass post-on-channel-audit-log (audit-log)
  ((slack-channel :initarg :channel
            :reader slack-channel)
   (text :initarg :text
            :reader slack-text)
   (ts :initarg :ts
       :reader %created-at))
  (:default-initargs :ts (get-universal-time))
  (:metaclass persistent-class))

(defun latest-slack-token (company)
  (let ((token
         (car (sort (slack-tokens-for-company company)
                    #'> :key '%created-at))))
    (when token
     (values (access-token token) token))))

(define-condition slack-error (error)
  ((response :initarg :response
             :reader slack-error-response)))

(defun slack-instance ()
  (#_getInstance #,com.slack.api.Slack))

(defun slack-methods (&key token)
  (check-type token string)
  (#_methods (slack-instance) token))

(defmethod print-object ((e slack-error) out)
  (with-slots (response) e
    (format out "Slack error with response: ~a" response)))

(defun check-slack-ok (response &optional audit-log)
  (unless (equal "true" (#_toString (#_isOk response)))
    (let ((response (#_getError response)))
      (when audit-log
       (with-transaction ()
         (setf (audit-log-error audit-log) response)))
      (error 'slack-error :response response))))

(defun slack-post-on-channel (&key channel text token company)
  (let ((methods (slack-methods :token token)))
   (let ((audit-log (make-instance 'post-on-channel-audit-log
                                    :company company
                                    :channel channel
                                    :text text)))
     (let ((builder (#_builder #,com.slack.api.methods.request.chat.ChatPostMessageRequest)))
       (#_channel builder channel)
       (#_text builder text)
       (let ((response (#_chatPostMessage methods (#_build builder))))
         (check-slack-ok response audit-log)
         t)))))

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
