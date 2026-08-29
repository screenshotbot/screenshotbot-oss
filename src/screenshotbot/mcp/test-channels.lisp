;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/test-channels
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/mcp/channels
                #:+max-channels+)
  (:import-from #:screenshotbot/mcp/test-util
                #:add-channel
                #:call-tool-as
                #:caller
                #:company
                #:decode
                #:field
                #:token
                #:token-with
                #:tool-text
                #:user)
  (:import-from #:screenshotbot/model/channel
                #:channel
                #:channel-slack-channels)
  (:import-from #:screenshotbot/model/company
                #:company-channels)
  (:documentation "The list_channels and update_slack_channels tools."))
(in-package :screenshotbot/mcp/test-channels)

(util/fiveam:def-suite)

(defun list-channels-as (token)
  (tool-text (call-tool-as token "list_channels")))

(test listing-channels-returns-them-as-json-with-names-and-urls
  (with-fixture caller ()
    (add-channel "beta")
    (add-channel "alpha")
    (multiple-value-bind (text result) (list-channels-as token)
      ;; No isError on a successful call -- the spec defaults it, and
      ;; saying "false" in CL-JSON would mean saying null.
      (is-false (field result "isError"))
      (let ((channels (decode text)))
        (is (equal 2 (length channels)))
        ;; Sorted, so the output does not reshuffle between calls for no
        ;; reason a reader could see.
        (is (equal "alpha" (field (first channels) "name")))
        (is (equal "beta" (field (second channels) "name")))
        (is-true (str:containsp "/channels/"
                                (field (first channels) "url")))))))

(test an-account-with-no-channels-gets-an-empty-list-not-an-error
  "A model handles [] fine; it handles a tool failure by giving up."
  (with-fixture caller ()
    (multiple-value-bind (text result) (list-channels-as token)
      (is-false (field result "isError"))
      (is (equal "[]" (str:trim text))))))

(test channels-belonging-to-another-account-are-not-listed
  (with-fixture caller ()
    (add-channel "ours")
    (let ((other (make-instance 'screenshotbot/model/company:company
                                :name "someone else")))
      (make-instance 'channel :name "theirs" :company other))
    (let ((text (list-channels-as token)))
      (is-true (str:containsp "ours" text))
      (is-false (str:containsp "theirs" text)))))

(test a-truncated-listing-says-so
  "A model that cannot see the cut will report a partial list as the
whole one."
  (with-fixture caller ()
    (dotimes (i 3)
      (add-channel (format nil "channel-~a" i)))
    (let ((text (progv (list '+max-channels+) (list 2)
                  (list-channels-as token))))
      (is-true (str:containsp "Showing the first 2 of 3 channels" text))
      (is-true (str:containsp "channel-0" text))
      (is-false (str:containsp "channel-2" text)))))

(test an-untruncated-listing-says-nothing-about-truncation
  (with-fixture caller ()
    (add-channel "only-one")
    (let ((text (list-channels-as token)))
      (is-false (str:containsp "Showing the first" text)))))

;; ----------------------------------------------------------------------
;; Slack channels
;; ----------------------------------------------------------------------

(defun slack-channels-of (text name)
  "The slackChannels reported for the channel called NAME."
  (let ((entry (find name (decode text)
                     :key (lambda (entry) (field entry "name"))
                     :test #'equal)))
    (coerce (field entry "slackChannels") 'list)))

(test a-channel-reports-the-slack-channels-notified-for-it
  (with-fixture caller ()
    (setf (channel-slack-channels (add-channel "web"))
          (list "eng" "web-ui"))
    (is (equal '("#eng" "#web-ui")
               (slack-channels-of (list-channels-as token) "web")))))

(test slack-channel-names-are-reported-with-a-leading-hash
  "They are stored bare -- the settings page strips the '#' on save and
only SEND-TASK puts it back -- so passing them through unchanged would
report `eng' for what everyone calls `#eng'."
  (with-fixture caller ()
    (setf (channel-slack-channels (add-channel "web")) (list "eng"))
    (is (equal '("#eng")
               (slack-channels-of (list-channels-as token) "web")))))

(test a-slack-name-already-carrying-a-hash-is-not-doubled
  (with-fixture caller ()
    (setf (channel-slack-channels (add-channel "web")) (list "#eng"))
    (is (equal '("#eng")
               (slack-channels-of (list-channels-as token) "web")))))

(test a-channel-notifying-nobody-reports-an-empty-list-not-null
  "CL-JSON renders an empty list as null, and a model told `null' has been
told something quite different from `nobody is notified'."
  (with-fixture caller ()
    (add-channel "quiet")
    (let ((entry (first (decode (list-channels-as token)))))
      (is (equal "quiet" (field entry "name")))
      ;; The decoder gives NIL for both [] and null, so the encoded text is
      ;; the only place the difference is visible.
      (is-true (str:containsp "\"slackChannels\":[]"
                              (list-channels-as token))))))

;; ----------------------------------------------------------------------
;; update_slack_channels
;; ----------------------------------------------------------------------

(defun update-slack-as (token name slack)
  (tool-text (call-tool-as token "update_slack_channels"
                           (list (cons "channel" name)
                                 (cons "slack_channels" slack)))))

(test setting-slack-channels-replaces-the-list-and-reports-it-back
  (with-fixture caller ()
    (setf (channel-slack-channels (add-channel "web")) (list "old"))
    (multiple-value-bind (text result)
        (update-slack-as (token-with '("api:read" "api:write")) "web" "#eng, #releases")
      (is-false (field result "isError"))
      (let ((entry (decode text)))
        (is (equal "web" (field entry "name")))
        ;; Replaced, not added to -- the old one is gone.
        (is (equal '("#eng" "#releases")
                   (coerce (field entry "slackChannels") 'list)))))))

(test the-change-is-what-list-channels-then-reports
  "The write and the read have to agree, or a model checking its own work
sees something it did not do."
  (with-fixture caller ()
    (add-channel "web")
    (update-slack-as (token-with '("api:read" "api:write")) "web" "#eng")
    (is (equal '("#eng")
               (slack-channels-of (list-channels-as token) "web")))))

(test names-are-stored-the-way-the-settings-page-stores-them
  "Both doors write the same slot. If they normalise differently, which
one you came through starts to matter."
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (update-slack-as (token-with '("api:read" "api:write")) "web" " #eng , releases ")
      (is (equal '("eng" "releases") (channel-slack-channels channel))))))

(test an-empty-value-stops-notifying-anyone
  "The reason SLACK_CHANNELS is declared :allow-empty. Without it the
required-argument check makes this unexpressible."
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (setf (channel-slack-channels channel) (list "eng"))
      (multiple-value-bind (text result) (update-slack-as (token-with '("api:read" "api:write")) "web" "")
        (is-false (field result "isError"))
        (is (equal nil (channel-slack-channels channel)))
        (is-true (str:containsp "\"slackChannels\":[]" text))))))

(test updating-slack-channels-needs-the-write-scope
  "The endpoint only asks for api:read, whose consent line promises the
client will read."
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (multiple-value-bind (text result)
          (update-slack-as (token-with '("api:read")) "web" "#eng")
        (is-true (field result "isError"))
        (is-true (str:containsp "api:write" text))
        ;; And nothing was written.
        (is (equal nil (channel-slack-channels channel)))))))

(test a-guest-may-list-channels-but-may-not-change-them
  "CAN-VIEWER-EDIT, not CAN-VIEWER-VIEW: holding the write scope says the
user agreed to let the client write, not that the user may."
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (roles:ensure-has-role company user 'roles:guest)
      (multiple-value-bind (text result)
          (update-slack-as (token-with '("api:read" "api:write")) "web" "#eng")
        (is-true (field result "isError"))
        (is-true (str:containsp "permission" text))
        (is (equal nil (channel-slack-channels channel)))))))

(test updating-an-unknown-channel-is-a-tool-error
  (with-fixture caller ()
    (multiple-value-bind (text result)
        (update-slack-as (token-with '("api:read" "api:write")) "no-such-channel" "#eng")
      (declare (ignore text))
      (is-true (field result "isError")))))

(test another-accounts-channel-cannot-be-updated-by-name
  "Lookup is scoped to the caller's company, so a name that exists
elsewhere has to read as simply absent."
  (with-fixture caller ()
    (let* ((other (make-instance 'screenshotbot/model/company:company
                                 :name "someone else"))
           (theirs (make-instance 'channel :name "theirs" :company other)))
      (push theirs (company-channels other))
      (multiple-value-bind (text result)
          (update-slack-as (token-with '("api:read" "api:write")) "theirs" "#mine-now")
        (declare (ignore text))
        (is-true (field result "isError"))
        (is (equal nil (channel-slack-channels theirs)))))))

(test too-many-slack-channels-is-a-tool-error-not-an-assertion-failure
  "The dashboard asserts on this. An assertion reaching a tool call is a
500 the model can do nothing with."
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (multiple-value-bind (text result)
          (update-slack-as (token-with '("api:read" "api:write")) "web"
                           (str:join "," (loop for i below 120
                                               collect (format nil "c~a" i))))
        (is-true (field result "isError"))
        (is-true (str:containsp "too many" text))
        (is (equal nil (channel-slack-channels channel)))))))
