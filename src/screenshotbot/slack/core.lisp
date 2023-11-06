;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/slack/core
  (:use #:cl
        #:alexandria
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
  (:import-from #:screenshotbot/model/auto-cleanup
                #:register-auto-cleanup)
  (:import-from #:screenshotbot/events
                #:with-event)
  (:import-from #:screenshotbot/audit-log
                #:audit-log-error
                #:base-audit-log
                #:audit-logs-for-company)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:util/store/store
                #:defindex)
  (:export
   #:slack-token
   #:latest-slack-token
   #:slack-error-code
   #:slack-message-failed
   #:slack-post-on-channel
   #:slack-methods
   #:audit-log
   #:slack-audit-logs-for-company
   #:post-on-channel-audit-log
   #:audit-log-error)
  ;; forward decls
  (:export #:find-or-create-slack-config)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/slack/core)

(defindex +company-index+
  'hash-index
  :slot-name 'company
  :test 'eql)

(with-class-validation
 (defclass slack-token (store-object)
   ((access-token
     :initarg :access-token
     :initform nil
     :reader access-token)
    (company
     :initarg :company
     :accessor slack-token-company
     :index +company-index+
     :index-reader slack-tokens-for-company)
    (ts
     :initform (get-universal-time)
     :initarg :ts
     :reader %created-at))
   (:metaclass persistent-class)))

(with-class-validation
 (defclass audit-log (base-audit-log)
   ((company :index-type hash-index
             :index-reader %slack-audit-logs-for-company
             :documentation "Deprecated slot for company")
    (err :initform nil
         :accessor %audit-log-error))
   (:metaclass persistent-class)))

(defmethod audit-log-error ((audit-log audit-log))
  (or
   (%audit-log-error audit-log)
   (call-next-method)))

(defmethod audit-logs-for-company :around (company (type (eql 'audit-log)))
  (append
   (call-next-method)

   ;; TODO: This is a migration, delete after thirty days!
   (let ((logs (%slack-audit-logs-for-company company)))
     ;; the BKNR datastore indices is super buggy for this
     ;; index. Probably because of the inheritence.
     (let ((hash-table (make-hash-table)))
       (loop for log in logs
             unless (bknr.datastore::object-destroyed-p log)
               do (setf (gethash log hash-table) t))
       (sort (alexandria:hash-table-keys hash-table)
             #'> :key #'bknr.datastore:store-object-id)))))

(defun slack-audit-logs-for-company (company)
  (audit-logs-for-company company 'audit-log))

(defclass post-on-channel-audit-log (audit-log)
  ((slack-channel :initarg :channel
            :reader slack-channel)
   (text :initarg :text
         :reader slack-text)
   ;; TODO: this is unnecessary, it's already available in
   ;; base-audit-log.
   (ts :initarg :ts
       :reader %created-at))
  (:default-initargs :ts (get-universal-time))
  (:metaclass persistent-class))

(register-auto-cleanup 'audit-log :timestamp #'%created-at)

(defun latest-slack-token (company)
  (let ((token
         (car (sort (copy-list (slack-tokens-for-company company))
                    #'> :key '%created-at))))
    (when token
     (values (access-token token) token))))

(define-condition slack-error (error)
  ((response :initarg :response
             :reader slack-error-response)))

(defmethod print-object ((e slack-error) out)
  (with-slots (response) e
    (format out "Slack error with response: ~a" response)))

(defun check-slack-ok (response &optional audit-log)
  (unless (a:assoc-value response :ok)
    (let ((response (a:assoc-value response :error)))
      (when audit-log
       (with-transaction ()
         (setf (audit-log-error audit-log) response)))
      (error 'slack-error :response response))))

(defun slack-request (&key (method :post)
                        token
                        parameters
                        url)
  (multiple-value-bind (body ret)
      (util/request:http-request
       (format nil "https://slack.com~a" url)
       :additional-headers (when token
                             `(("Authorization" . ,(format nil "Bearer ~a" token))))
       :method method
       :ensure-success t
       :parameters parameters
       :want-string t)
    (let ((body (json:decode-json-from-string body)))
      (values
       body
       ret))))

(defun slack-post-on-channel (&key channel text token company
                                blocks
                                (unfurl-urls nil))
  (with-event (:slack)
   (let ((audit-log (make-instance 'post-on-channel-audit-log
                                   :company company
                                   :channel channel
                                   :text text)))
     (let ((response (slack-request
                      :url "/api/chat.postMessage"
                      :token token
                      :parameters
                      (remove-if #'null `(("channel" . ,channel)
                                          ("unfurl_links" . ,(if unfurl-urls "true" "false"))
                                          ,(when text
                                             `("text" . ,text))
                                          ,(when blocks
                                             `("blocks" . ,(json:encode-json-to-string blocks))))))))
       (check-slack-ok response audit-log)))))

(defhandler (nil :uri "/slack-app-redirect") (code state)
  (declare (ignore state))
  (with-plugin (slack-plugin)
   (cond
     ((null code)
      (hex:safe-redirect "/settings/slack"))
     (t
      (multiple-value-bind (response)
          (slack-request
           :url "/api/oauth.v2.access"
           :parameters `(("client_id" . ,(client-id slack-plugin))
                         ("client_secret" . ,(client-secret slack-plugin))
                         ("code" . ,code)))
        (check-slack-ok response)
        (let ((access-token (assoc-value response :access--token)))
          (assert access-token)
          (let ((token (make-instance 'slack-token
                                      :access-token access-token
                                      :company (current-company))))
            (with-transaction ()
              (setf (access-token (find-or-create-slack-config (current-company)))
                    token)))

          (hex:safe-redirect "/settings/slack")))))))

;; (slack-post-on-channel :channel "#random" :text "foobar2"
;;                        :methods (slack-methods :token "xoxb-1422927031269-1607469573089-WIpznshstwSK3yBTtnDhj4de"))
