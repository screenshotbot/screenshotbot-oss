;;;; -*- encoding: utf-8 -*-
;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/documentation/doc
    (:use #:cl
          #:alexandria)
  (:import-from #:screenshotbot/server
                #:defhandler))


(markup:enable-reader)

(defhandler (nil :uri "/documentation") ()
  (hunchentoot:redirect "https://docs.screenshotbot.io/docs/"))

(defhandler (nil :uri "/documentation/:page") (page)
  (hunchentoot:redirect (format nil "https://docs.screenshotbot.io/docs/~a" page)))
