;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/test-masks
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/mcp/masks
                #:+max-masked-screenshots+)
  (:import-from #:screenshotbot/mcp/test-util
                #:add-channel
                #:call-tool-as
                #:caller
                #:company
                #:decode
                #:field
                #:token
                #:token-with
                #:tool-text)
  (:import-from #:screenshotbot/model/channel
                #:channel
                #:set-channel-screenshot-mask)
  (:import-from #:screenshotbot/model/company
                #:company-channels)
  (:import-from #:screenshotbot/model/image
                #:mask-rect)
  (:documentation "The list_masks tool."))
(in-package :screenshotbot/mcp/test-masks)

(util/fiveam:def-suite)

(defun rect (&key (left 0) (top 0) (width 10) (height 10))
  (make-instance 'mask-rect :left left :top top :width width :height height))

(defun list-masks-as (token name)
  (tool-text (call-tool-as token "list_masks" (list (cons "channel" name)))))

(defun entries (text)
  (decode (first (str:lines text))))

(defun entry-for (text screenshot)
  (find screenshot (entries text)
        :key (lambda (entry) (field entry "screenshot"))
        :test #'equal))

(test masks-are-reported-per-screenshot-as-rectangles
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (set-channel-screenshot-mask channel "home"
                                   (list (rect :left 5 :top 6 :width 7 :height 8)))
      (multiple-value-bind (text result) (list-masks-as token "web")
        (is-false (field result "isError"))
        (let* ((entry (entry-for text "home"))
               (mask (first (coerce (field entry "masks") 'list))))
          (is (equal 5 (field mask "left")))
          (is (equal 6 (field mask "top")))
          (is (equal 7 (field mask "width")))
          (is (equal 8 (field mask "height"))))))))

(test several-masks-on-one-screenshot-are-all-reported
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (set-channel-screenshot-mask channel "home"
                                   (list (rect :left 1) (rect :left 2)))
      (let ((entry (entry-for (list-masks-as token "web") "home")))
        (is (equal 2 (length (field entry "masks"))))))))

(test a-rectangle-dragged-backwards-is-reported-the-way-it-looks
  "A mask dragged up and to the left is stored with a negative width and
height. Reporting the slots raw would describe a rectangle nobody could
draw, and a model asked to adjust it would work from the wrong corner."
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (set-channel-screenshot-mask channel "home"
                                   (list (rect :left 30 :top 40
                                               :width -10 :height -20)))
      (let* ((entry (entry-for (list-masks-as token "web") "home"))
             (mask (first (coerce (field entry "masks") 'list))))
        (is (equal 20 (field mask "left")))
        (is (equal 20 (field mask "top")))
        (is (equal 10 (field mask "width")))
        (is (equal 20 (field mask "height")))))))

(test screenshots-are-reported-in-a-stable-order
  "An alist that reorders between calls makes a model think something
changed."
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (set-channel-screenshot-mask channel "zebra" (list (rect)))
      (set-channel-screenshot-mask channel "alpha" (list (rect)))
      (let ((names (mapcar (lambda (entry) (field entry "screenshot"))
                           (entries (list-masks-as token "web")))))
        (is (equal '("alpha" "zebra") names))))))

(test a-screenshot-whose-masks-were-cleared-is-not-reported
  "Clearing leaves the entry behind with an empty list rather than
removing it, so without the filter this would report an unmasked
screenshot as masked."
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (set-channel-screenshot-mask channel "home" (list (rect)))
      (set-channel-screenshot-mask channel "home" nil)
      (is (equal "[]" (str:trim (list-masks-as token "web")))))))

(test a-channel-with-no-masks-gets-an-empty-list-not-an-error
  "A model handles [] fine; it handles a tool failure by giving up."
  (with-fixture caller ()
    (add-channel "web")
    (multiple-value-bind (text result) (list-masks-as token "web")
      (is-false (field result "isError"))
      (is (equal "[]" (str:trim text))))))

(test listing-masks-for-an-unknown-channel-is-a-tool-error
  (with-fixture caller ()
    (multiple-value-bind (text result) (list-masks-as token "no-such-channel")
      (declare (ignore text))
      (is-true (field result "isError")))))

(test another-accounts-masks-are-not-reachable-by-channel-name
  (with-fixture caller ()
    (let* ((other (make-instance 'screenshotbot/model/company:company
                                 :name "someone else"))
           (theirs (make-instance 'channel :name "theirs" :company other)))
      (push theirs (company-channels other))
      (set-channel-screenshot-mask theirs "secret-screen" (list (rect)))
      (multiple-value-bind (text result) (list-masks-as token "theirs")
        (is-true (field result "isError"))
        (is-false (str:containsp "secret-screen" text))))))

(test a-truncated-listing-says-so
  "A model that cannot see the cut reports a partial list as the whole one."
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (dotimes (i 3)
        (set-channel-screenshot-mask channel (format nil "screen-~a" i)
                                     (list (rect))))
      (let ((text (progv (list '+max-masked-screenshots+) (list 2)
                    (list-masks-as token "web"))))
        (is-true (str:containsp "Showing the first 2 of 3 masked screenshots"
                                text))
        (is-false (str:containsp "screen-2" text))))))

(test an-untruncated-listing-says-nothing-about-truncation
  (with-fixture caller ()
    (set-channel-screenshot-mask (add-channel "web") "home" (list (rect)))
    (is-false (str:containsp "Showing the first" (list-masks-as token "web")))))

(test listing-masks-without-the-scope-never-reaches-it
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (set-channel-screenshot-mask channel "secret-screen" (list (rect)))
      (multiple-value-bind (body status)
          (call-tool-as (token-with '("profile")) "list_masks"
                        (list (cons "channel" "web")))
        (is (equal 403 status))
        (is-false (str:containsp "secret-screen" body))))))
