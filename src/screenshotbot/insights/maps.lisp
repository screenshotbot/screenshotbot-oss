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
                #:recorder-run-channel
                #:%created-at)
  (:import-from #:screenshotbot/model/recorder-run
                #:run-screenshot-map)
  (:local-nicknames (#:screenshot-map #:screenshotbot/model/screenshot-map)))
(in-package :screenshotbot/insights/maps)

(defun %map-to-list (map)
  (loop for key being the hash-keys of map
            using (hash-value val)
          collect (list key val)))

(defun date-map (runs)
  "From runs create a map of (<date>, runs)"
  (let ((map (make-hash-table :test #'equal)))
    (dolist (run runs)
      (pushnew run (gethash (format-date (%created-at run)) map)))
    (%map-to-list map)))

(defun channel-map (runs)
  "From a list of runs, generate <channel>,runs"
  (let ((map (make-hash-table)))
    (dolist (run runs)
      (pushnew run (gethash (recorder-run-channel run) map)))
    (%map-to-list map)))

(defun date-channel-map (runs)
  "From a list of runs, generate <date>,<channel>,runs"
  (loop for (date date-runs) in (date-map runs)
        for channel-map = (channel-map date-runs)
        appending
        (loop for item in channel-map
              collect (list* date item))))

(defun max-run-length (runs)
  (loop for run in runs
        for map = (run-screenshot-map run)
        if map
          maximizing (fset:size (screenshot-map:to-map
                                 map))))

(defun date-channel-maxLength (date-channel-map)
  "From a DATE-CHANNEL-MAP, compute <date>,<channel>,<max length of run
we saw in that day>. If a channel didn't trigger on a specific day, it
won't be listed here."
  (loop for (date channel runs) in date-channel-map
        collect
        (list date channel (max-run-length runs))))


