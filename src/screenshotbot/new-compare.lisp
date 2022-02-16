;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/new-compare
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/template
                #:dashboard-head
                #:app-template)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/new-compare)

(markup:enable-reader)

(defhandler (nil :uri "/new-compare") ()
  <html>
    <dashboard-head />
    <div class= "split-container">
      <div class= "new-compare split">
        <div class= "image-list" >
          hello
        </div>
        <div class= "image-details">
          world
        </div>
      </div>

      <script src= "/assets/js/dashboard.js" />
    </div>

  </html>)
