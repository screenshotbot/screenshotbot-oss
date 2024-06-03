;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mailer
  (:nicknames :core/installation/mailer)
  (:use :cl)
  (:import-from #:util/threading
                #:make-thread
                #:max-pool)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:export
   #:background-mailer
   #:host
   #:local-smtp-mailer
   #:noop-mailer
   #:send-mail
   #:smtp-mailer
   #:mailer*
   #:mailer
   #:wrap-template))
(in-package :core/installation/mailer)

(defclass mailer ()
  ((from :initarg :from
         :accessor from)))

(defclass noop-mailer (mailer)
  ()
  (:documentation "A mailer that does nothing"))

(defclass smtp-mailer (mailer)
  ((host :initarg :hostname
         :accessor host)
   (port :initarg :port
         :initform 587
         :accessor port)
   (ssl :initarg :ssl
        :initform t
        :accessor ssl)
   (user :initarg :user
         :accessor user)
   (password :initarg :password
             :accessor password)))

(defclass background-mailer (mailer)
  ((delegate :initarg :delegate
             :accessor delegate)))

(defclass local-smtp-mailer (smtp-mailer)
  ()
  (:default-initargs
   :hostname "localhost"
   :port 25
   :ssl nil)
  (:documentation "A mailer that uses the local SMTP port. We expect
  that this SMTP server shouldn't require authentication."))

(defgeneric send-mail (mailer &key from subject to html-message bcc
                                reply-to
                                display-name))

(defmethod authentication ((mailer local-smtp-mailer))
  nil)

(defmethod authentication ((mailer smtp-mailer))
  `(:plain ,(user mailer) ,(password mailer)))

(defmethod send-mail ((mailer noop-mailer) &key &allow-other-keys)
  (declare (ignore mailer)))

(defun parse-from (from display-name)
  "If the from is provides as Foo Bar <foo@bar>, parse out the from and
 display-name from it. This is required for some SMTP servers, but not
 all."
  (cond
    (display-name
     (values from display-name))
    (t
     (multiple-value-bind (parse parts)
         (cl-ppcre:scan-to-strings
          "(.*)<(.*)>.*" from)
       (cond
         (parse
          (values
           (str:trim (elt parts 1))
           (str:trim (elt parts 0))))
         (t
          (values from display-name)))))))

(defun fix-email-list (emails)
  (cond
    ((listp emails)
     (loop for email in emails
           collect (parse-from email nil)))
    (t
     (parse-from emails nil))))

(defmethod wrap-template (mailer html-message)
  html-message)

(defmethod send-mail ((mailer smtp-mailer)
                      &rest args
                      &key from subject to html-message
                        display-name
                        reply-to
                        bcc)
  (log:info "Sending mail ~S" args)
  (restart-case
      (multiple-value-bind (from display-name)
          (parse-from from display-name)
       (cl-smtp:send-email
        (host mailer)
        (or from (from mailer))
        (fix-email-list to)
        subject
        (util/html2text:html2text html-message)
        :ssl (ssl mailer)
        :bcc (fix-email-list bcc)
        :port (port mailer)
        :reply-to reply-to
        :display-name display-name
        :authentication (authentication mailer)
        :port (port mailer)
        :html-message (markup:write-html (wrap-template mailer html-message))))
    (dont-send-the-mail ()
      nil)))

(defvar *mailer-pool* (make-instance 'max-pool))

(defmethod send-mail ((mailer background-mailer) &rest args)
  (let ((promise (lparallel:promise)))
    (prog1
        promise
      (make-thread
       (lambda ()
         (lparallel:fulfill promise
           (apply #'send-mail (delegate mailer) args)))
       :pool *mailer-pool*
       :name "email thread"))))

(defgeneric mailer (installation))

(defun mailer* (&optional (installation *installation*))
  (mailer installation))
