;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/compare-branches
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/template
                #:app-template)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:nibble
                #:nibble))
(in-package :screenshotbot/dashboard/compare-branches)

(named-readtables:in-readtable markup:syntax)

(defhandler (nil :uri "/compare-branches") ()
  (assert (gk:check :compare-branches (auth:current-company)))
  (let ((action (nibble (sha1 sha2)
                  (declare (ignore sha1 sha2))
                  (error "unimpl"))))
    <simple-card-page form-action=action >
      <div class= "card-header">
        <h4>Compare branches or commits</h4>
      </div>
      <div class= "card-body">
        <div class= "mb-2">
          <label for= "sha1" class= "form-label" >
            First SHA/branch
          </label>
          <input type= "text" class= "form-control" id= "sha1" name= "sha1" placeholder= "abcdef0102" />
        </div>

        <div class= "mb-2">
          <label for= "sha2" class= "form-label" >
            Second SHA/branch
          </label>
          <input type= "text" class= "form-control" id= "sha2" name= "sha2" placeholder= "abcdef0102"/>
        </div>

      </div>
      <div class= "card-footer">
        <input type= "submit" value= "Compare" class= "btn btn-primary" />
        <a href= "/" class= "btn btn-secondary" >Cancel</a>
      </div>
    </simple-card-page>))


