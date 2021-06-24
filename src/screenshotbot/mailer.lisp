(pkg:define-package :screenshotbot/mailer
    (:use #:cl
          #:alexandria)
  (:export #:noop-mailer
           #:local-smtp-mailer))

(defclass mailer ()
  ((from :initarg :from
         :accessor from)))

(defclass noop-mailer (mailer)
  ()
  (:documentation "A mailer that does nothing"))

(defclass local-smtp-mailer (mailer)
  ((port :initarg :port
         :initform 25
         :accessor port))
  (:documentation "A mailer that uses the local SMTP port. We expect
  that this SMTP server shouldn't require authentication."))

(defgeneric send-mail (mailer &key from subject to html-message))

(defmethod send-mail ((mailer local-smtp-mailer)
                      &key from subject to html-message)
  (restart-case
      (unless util:*disable-emails*
       (cl-smtp:send-email
        "localhost"
        (or from (from mailer))
        to
        subject
        (util:html2text html-message)
        :port (port mailer)
        :html-message (markup:write-html html-message)))
    (dont-send-the-mail ()
      nil)))
