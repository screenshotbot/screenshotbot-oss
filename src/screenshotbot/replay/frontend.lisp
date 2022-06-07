;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/frontend
  (:use #:cl)
  (:nicknames :screenshotbot/pro/replay/frontend)
  (:shadow #:log)
  (:import-from #:screenshotbot/replay/browser-config
                #:browser-config
                #:dimensions
                #:height
                #:width
                #:browser-type
                #:mobile-emulation
                #:browser-config-name)
  (:import-from #:markup
                #:write-html
                #:deftag)
  (:import-from #:screenshotbot/pro/replay/replay-acceptor
                #:call-with-hosted-snapshot)
  (:import-from #:screenshotbot/replay/core
                #:uuid
                #:*replay-logs*
                #:load-url)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/webdriver
                #:chrome
                #:firefox
                #:take-screenshot
                #:with-webdriver)
  (:import-from #:webdriver-client
                #:http-post-check)
  (:import-from #:screenshotbot/webdriver/impl
                #:call-with-webdriver)
  (:import-from #:screenshotbot/webdriver/screenshot
                #:full-page-screenshot)
  (:import-from #:screenshotbot/pro/replay/services
                #:selenium-server
                #:selenium-server-url)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:browser-config
   #:dimensions
   #:height
   #:width
   #:browser-type
   #:mobile-emulation
   #:browser-config-name
   #:type))
(in-package :screenshotbot/replay/frontend)

(markup:enable-reader)

(defclass screenshot ()
  ((key :initarg :key
        :reader screenshot-file-key)
   (title :initarg :title
          :reader screenshot-title)))

(defparameter *default-browser-configs*
  (list
   (make-instance 'browser-config
                  :name "Firefox"
                  :type 'firefox)
   (make-instance 'browser-config
                  :name "Chrome"
                  :dimensions (make-instance 'dimensions :width 1280 :height 800)
                  :type 'chrome)
   (make-instance 'browser-config
                  :name "Chrome Nexus 5 emulation"
                  :type 'chrome
                  :mobile-emulation "Nexus 6P")
   (make-instance 'browser-config
                  :name "Chrome Pixel 2 emulation"
                  :type 'chrome
                  :mobile-emulation "Pixel 2")))

(defclass job ()
  ((logs :initarg :logs
         :reader job-logs)
   (url :initarg :url
        :reader url)
   (sleep :initarg :sleep
          :initform 5
          :reader sleep-time)
   (screenshots :initform nil
                :accessor screenshots)
   (browser-configs :initarg :browser-configs
                    :initform *default-browser-configs*
                    :reader browser-configs)
   (donep :initform nil
          :accessor donep)))


(defun arr (x)
  (make-array (length x)
              :initial-contents x))
