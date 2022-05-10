;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/mailer
    (:use #:cl #:alexandria)
  (:export
   #:noop-mailer
   #:smtp-mailer
   #:local-smtp-mailer
   #:background-mailer
   #:send-mail
   #:host))
(in-package :screenshotbot/mailer)

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

(defgeneric send-mail (mailer &key from subject to html-message bcc))

(defmethod authentication ((mailer local-smtp-mailer))
  nil)

(defmethod authentication ((mailer smtp-mailer))
  `(:plain ,(user mailer) ,(password mailer)))

(defmethod send-mail ((mailer noop-mailer) &key &allow-other-keys)
  (declare (ignore mailer)))

(defmethod send-mail ((mailer smtp-mailer)
                      &key from subject to html-message
                        bcc)
  (restart-case
      (unless util:*disable-emails*
       (cl-smtp:send-email
        (host mailer)
        (or from (from mailer))
        to
        subject
        (util:html2text html-message)
        :ssl (ssl mailer)
        :bcc bcc
        :port (port mailer)
        :authentication (authentication mailer)
        :port (port mailer)
        :html-message (markup:write-html html-message)))
    (dont-send-the-mail ()
      nil)))

(defmethod send-mail ((mailer background-mailer) &rest args)
  (util:make-thread
   (lambda ()
     (apply #'send-mail (delegate mailer) args))))
