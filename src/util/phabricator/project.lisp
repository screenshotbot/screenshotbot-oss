;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/phabricator/project
  (:use #:cl)
  (:import-from #:util/phabricator/conduit
                #:call-conduit
                #:phab-instance)
  (:import-from #:alexandria
                #:assoc-value))
(in-package :util/phabricator/project)

(defmethod project-phid ((phab phab-instance)
                         name)
  (assoc-value
   (cdr
    (first
     (assoc-value
      (assoc-value
       (call-conduit phab
                     "project.query"
                     `(("names" . #(,name))))
       :result)
      :data)))
   :phid))

;; (project-phid (util/phabricator/conduit::phab-test) "screenshotbot")


