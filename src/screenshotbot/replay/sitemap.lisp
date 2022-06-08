;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/sitemap
  (:use #:cl)
  (:nicknames :screenshotbot/pro/replay/sitemap)
  (:import-from #:screenshotbot/replay/core
                #:write-replay-log)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:parse-sitemap))
(in-package :screenshotbot/replay/sitemap)

(defun parse-sitemap (url)
  "Gets the list of all URLS in a given sitemap file"
  (write-replay-log "Fetching sitemap: ~a" url)
  (restart-case
      (remove-duplicates
       (let* ((content (dex:get url))
              (root (xmls:parse content))
              (urls (xmls:node-children root)))
         (remove-if #'null
          (loop for url in urls
                collect
                (loop for attr in (xmls:node-children url)
                      if (string-equal "loc" (xmls:node-name attr))
                        return (car (xmls:node-children attr))))))
       :test #'equal)
    (retry-parse-sitemap ()
      (parse-sitemap url))))


;; (parse-sitemap "https://rollins-edu-cla-staging.netlify.app/sitemap.xml")
