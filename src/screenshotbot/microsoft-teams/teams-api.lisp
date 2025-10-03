;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/microsoft-teams/teams-api
  (:use #:cl
        #:alexandria)
  (:export
   #:teams-post-adaptive-card
   #:make-adaptive-card
   #:teams-error
   #:teams-error-response))
(in-package :screenshotbot/microsoft-teams/teams-api)

(define-condition teams-error (error)
  ((response :initarg :response
             :reader teams-error-response))
  (:report (lambda (condition stream)
             (format stream "Teams API error: ~a" (teams-error-response condition)))))

(defun make-adaptive-card (&key text)
  "Create a simple Adaptive Card structure with the given text."
  (list
   (cons :type "message")
   (cons :attachments
         (vector
          (alexandria:alist-hash-table
           `((:content-type . "application/vnd.microsoft.card.adaptive")
             (:content . ((:$schema . "http://adaptivecards.io/schemas/adaptive-card.json")
                          (:type . "AdaptiveCard")
                          (:version . "1.4")
                          (:body . ,(vector
                                     (alexandria:alist-hash-table
                                      `((:type . "TextBlock")
                                        (:text . ,text)
                                        (:wrap . t)))))))))))))

(defun teams-post-adaptive-card (&key webhook-url card-payload)
  "Post an Adaptive Card to a Microsoft Teams channel via webhook.

   WEBHOOK-URL should be the full Power Automate workflow webhook URL
   (including the sig parameter).

   CARD-PAYLOAD should be an alist representing the Adaptive Card structure.
   You can use MAKE-ADAPTIVE-CARD for simple cards, or construct your own."
  (let ((content (json:encode-json-to-string card-payload)))
   (handler-case
       (multiple-value-bind (body status-code)
           (util/request:http-request
            webhook-url
            :method :post
            :content-type "application/json"
            :content content
            :want-string t
            :preserve-uri t)
         (unless (member status-code '(200 202))
           (error 'teams-error :response (format nil "HTTP ~a: ~a" status-code body)))
         (values body status-code))
     (error (e)
       (error 'teams-error :response (format nil "~a" e))))))
