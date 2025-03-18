;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/docs/recorder-run
  (:use #:cl)
  (:import-from #:screenshotbot/api/doc
                #:def-api-doc)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/docs/recorder-run)

(named-readtables:in-readtable markup:syntax)

(def-api-doc ("/api/run/:id" :input-type nil
              :title "Retrieve a run"
              :method :get
                             :output-type 'dto:run)
  <remark:md>
    Get the run associated with a given ID.
  </remark:md>)

