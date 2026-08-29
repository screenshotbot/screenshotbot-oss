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
                #:channel)
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
