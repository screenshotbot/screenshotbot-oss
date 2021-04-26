;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :util)

(markup:enable-reader)

(defvar *disable-emails* nil)
(defun parse-email-list (s)
  (cond
    ((stringp s)
     (mapcar 'str:trim (str:split "," s)))
    (t s)))

(defun make-mail-message-id ()
  (format nil "<~a@tdrhq.com>"
          (mongoid:oid-str (mongoid:oid))))

(defun send-mail (&key to bcc (subject "") message (from (error "specify from")) html-message
                    attachments
                    (message-id (make-mail-message-id))
                    reply-to)
  "HTML message must be a markup object. Returns the Message-id of the message"
  (check-type subject string)
  (loop for a in (alexandria:flatten (list attachments))
        if (or (pathnamep a) (stringp a))
          do (assert (path:-e a)))
  (restart-case
      (unless *disable-emails*
       (cl-smtp:send-email
        "localhost"
        from
        to
        subject
        (or message (html2text html-message))
        :bcc bcc
        :port (if (equal "thecharmer" (uiop:hostname))
                  2025
                  25)
        :reply-to reply-to
        :attachments attachments
        :extra-headers `(("Message-ID" ,message-id))
        :html-message (if html-message
                          (markup:write-html html-message)))
       (values message-id))
    (dont-send-the-mail ()
      nil)))
