;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/microsoft-teams
  (:use #:cl)
  (:import-from #:markup
                #:deftag)
  (:import-from #:nibble
                #:nibble))
(in-package :screenshotbot/dashboard/microsoft-teams)

(named-readtables:in-readtable markup:syntax)


(deftag microsoft-teams-card (&key channel)
  <div class= "card mt-3">
    <div class= "card-body">
      <h4 class= "card-title mb-3">
        Microsoft Teams worfklows
      </h4>

      <div class= "d-flex gap-2">
        <a href="#" class= "btn btn-primary">Add Workflow Webhook</a>
      </div>
    </div>
  </div>)

