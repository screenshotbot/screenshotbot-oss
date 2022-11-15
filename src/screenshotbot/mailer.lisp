;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/mailer
    (:use #:cl #:alexandria)
    (:import-from #:util/threading
                  #:ignore-and-log-errors)
    (:import-from #:screenshotbot/async
                  #:sb/future)
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
          :reply-to reply-to
          :display-name display-name
          :authentication (authentication mailer)
          :port (port mailer)
          :html-message (markup:write-html html-message))))
    (dont-send-the-mail ()
      nil)))

(defmethod send-mail ((mailer background-mailer) &rest args)
  (sb/future ()
    (ignore-and-log-errors ()
      (apply #'send-mail (delegate mailer) args))))
