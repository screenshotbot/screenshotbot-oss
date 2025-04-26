;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/commit-graph
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/sdk
                #:request)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/commit-graph)

(defmethod get-commit-graph-refs (api-context
                                  repo)
  (dto:decode-json
   (request
    api-context
    "/api/commit-graph/refs"
    :method :get
    :decode-response nil
    :parameters (list
                 (cons "repo-url" repo)))
   '(:list dto:git-ref)))

