;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/insights/maps
  (:use #:cl)
  (:import-from #:screenshotbot/insights/date
                #:format-date)
  (:import-from #:screenshotbot/user-api
                #:%created-at))
(in-package :screenshotbot/insights/maps)

(defun date-map (runs)
  "From runs create a map of (<date>, runs)"
  (let ((map (make-hash-table :test #'equal)))
    (dolist (run runs)
      (pushnew run (gethash (format-date (%created-at run)) map)))
    (loop for key being the hash-keys of map
            using (hash-value val)
          collect (list key val))))


