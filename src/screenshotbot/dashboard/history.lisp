;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/dashboard/history
    (:use #:cl
          #:alexandria
          #:../user-api
          #:../report-api
          #:../template
          #:../screenshot-api)
   (:import-from #:./run-page
                 #:commit)
   (:import-from #:bknr.datastore
                 #:store-object-with-id)
   (:import-from #:../server
                #:defhandler)
   (:import-from #:./run-page
                 #:history-page))


(markup:enable-reader)

(markup:deftag render-history (&key screenshots runs channel)
  <div>
    ,@ (loop for s in screenshots
    for r in runs
    collect
    (cond
    (s
    <div>
      <h4>,(screenshot-name s)</h4>
      <p>First seen in <commit repo= (channel-repo channel)
                               hash= (recorder-run-commit r) /></p>
      <img src=(image-public-url (screenshot-image s)) />

    </div>)
    (t
     <div>
       <h4>Deleted</h4>
     </div>)))
  </div>)

(defhandler (history-page :uri "/channel/:channel/history/:screenshot-name")
            (channel screenshot-name)
  (let ((channel (store-object-with-id (parse-integer channel))))
    (can-view! channel)
    (multiple-value-bind (screenshots runs) (get-screenshot-history channel screenshot-name)
      (app-template
       (render-history
        :screenshots screenshots
        :channel channel
        :runs runs)))))
