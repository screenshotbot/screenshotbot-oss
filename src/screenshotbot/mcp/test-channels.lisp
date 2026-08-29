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
                #:decode
                #:field
                #:token
                #:tool-text)
  (:import-from #:screenshotbot/model/channel
                #:channel
                #:channel-slack-channels)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:documentation "The list_channels tool."))
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
    (let ((other (make-instance 'company :name "someone else")))
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
