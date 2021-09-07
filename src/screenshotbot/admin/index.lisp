;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/admin/index
  (:use #:cl
        #:alexandria
        #:screenshotbot/admin/core
        #:screenshotbot/user-api
        #:screenshotbot/model/user))
(in-package :screenshotbot/admin/index)

(markup:enable-reader)


(defadminhandler (nil :uri "/admin") ()
  (let ((reload-nibble (nibble (:once t)
                         (confirmation-page
                          :yes (nibble ()
                                 (admin-reload))
                          :no "/admin"
                          "Danger: Are you sure you want to restart the server?"))))
    <app-template>
      <h1>Admin panel (Internal use only)</h1>
      <ul>

        ,@ (loop for (title . destination) in (reverse *index*)
                 collect
                 <li>
                   <a href= (hex:make-url destination)>,(progn title)</a>
                 </li>)
      <li>
    <a href= reload-nibble
       >Reload</a>
      </li>

    <li>
    <a href= (nibble () (do-snapshot)) >Create Datastore Snapshot </a>
    </li>
    </ul>
  </app-template>)
)


(defun do-snapshot ()
  (bknr.datastore:snapshot)
  (hex:safe-redirect "/admin"))

(defun admin-reload ()
  (snapshot)
  (asdf:load-system :screenshotbot)
  (snapshot)
  (hex:safe-redirect "/admin"))
