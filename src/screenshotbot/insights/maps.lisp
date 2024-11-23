;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/insights/maps
  (:use #:cl)
  (:import-from #:screenshotbot/insights/date
                #:increment-date
                #:list-dates
                #:format-date)
  (:import-from #:screenshotbot/user-api
                #:channel-name
                #:recorder-run-channel
                #:%created-at)
  (:import-from #:screenshotbot/model/recorder-run
                #:runs-for-company
                #:run-screenshot-map)
  (:import-from #:priority-queue
                #:pqueue-empty-p
                #:pqueue-front
                #:pqueue-pop
                #:pqueue-push
                #:make-pqueue)
  (:local-nicknames (#:screenshot-map #:screenshotbot/model/screenshot-map))
  (:export
   #:screenshot-count-map))
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

(defun %add-sentinal-date (list)
  "Adds one fake date to the end of the list"
  (let ((last-date (first (car (last list)))))
    (let ((sentinel-item (list (increment-date last-date) :channel 0)))
      (append
       list
       (list
        sentinel-item)))))

(defun date-to-screenshots-count (date-channel-maxLength &key (channel-expiration-window 30))
  (when date-channel-maxLength
    (let* ((date-channel-maxLength (%add-sentinal-date
                                    (sort
                                     (copy-list date-channel-maxLength)
                                     #'string<
                                     :key #'first)))
          (result)

          (channel-size (make-hash-table))
          ;; For every channel, track at what point we'll automatically
          ;; delete it from the running count.
          (channel-expiry (make-hash-table))
          ;; The queue might have outdated entries, so beware!
          (expiry-queue (make-pqueue #'string<))
          (curr-date (first (first date-channel-maxLength)))
          (size 0))
     (labels ((bump-expiry (channel date)
                ;;(log:debug "Bumping expiry for ~a to ~a" channel date)
                (setf (gethash channel channel-expiry) date)
                (pqueue-push channel date expiry-queue))
              (expire-entries (date)
                (unless (pqueue-empty-p expiry-queue)
                  (multiple-value-bind (channel expiry-date)
                      (pqueue-front expiry-queue)
                    (when (string< expiry-date date)
                      (pqueue-pop expiry-queue)
                      (when (equal expiry-date (gethash channel channel-expiry))
                        (let ((curr-size (gethash channel channel-size)))
                          (unless curr-size
                            (error "Hmm, expected to see a the channel-size since it's not expired yet"))
                          (decf size curr-size))
                        (remhash channel channel-expiry)
                        (remhash channel channel-size))
                      (expire-entries date))))))
       (loop for (date channel maxLength) in date-channel-maxLength do
         (bump-expiry channel (increment-date date channel-expiration-window))
         (expire-entries date)
         (let ((last-size (or (gethash channel channel-size) 0)))
           (loop while (string< curr-date date) do
             (push (list curr-date size) result)
             (setf curr-date (increment-date curr-date)))
        
           ;; At this point curr-date = date
           (decf size last-size)
           (setf (gethash channel channel-size) maxLength)
           (incf size (gethash channel channel-size)))))
     (nreverse result))))

(defun screenshot-count-map (runs)
  (date-to-screenshots-count (date-channel-maxLength (date-channel-map runs))))

(defun screenshot-count-map-by-filter (company prefix &key file)
  (let* ((runs (fset:convert 'list (runs-for-company company)))
         (runs (loop for run in runs
                     if (str:starts-with-p prefix (channel-name (recorder-run-channel run)))
                       collect run)))
    (let ((result (screenshot-count-map runs)))
      (when file
        (with-open-file (s file :direction :output)
          (loop for (date count) in result
                do (format s "~a,~a~%" date count))))
      result)))





