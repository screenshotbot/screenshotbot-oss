;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/figma/view
  (:use #:cl)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:screenshotbot/dashboard/compare
                #:associate-figma))
(in-package :screenshotbot/figma/view)

(named-readtables:in-readtable markup:syntax)

;; TODO: take in image etc.
(defun associate-figma ()

  <simple-card-page form-action="/figma/associate">
    <div class="form-group mb-3">
      <label for="figma-url" class="form-label">Figma Component URL</label>
      <input type="url" 
             class="form-control" 
             id="figma-url" 
             name="figma-url" 
             placeholder="https://www.figma.com/file/..." 
             required />
      <div class="form-text">
        Enter the URL of the Figma component or frame you want to associate
      </div>
    </div>
    <button type="submit" class="btn btn-primary">Associate with Figma</button>
  </simple-card-page>)

