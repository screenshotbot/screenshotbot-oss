;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/figma/dropdown
  (:use #:cl)
  (:import-from #:markup #:deftag)
  (:import-from #:gk #:check)
  (:import-from #:auth #:current-company)
  (:import-from #:screenshotbot/model/figma
                #:figma-link-url
                #:find-existing-figma-link)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-channel)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot-name)
  (:import-from #:nibble #:nibble)
  (:import-from #:core/ui/simple-card-page
                #:confirmation-page)
  (:import-from #:screenshotbot/figma/view
                #:associate-figma)
  (:import-from #:core/ui/mdi
                #:mdi))
(in-package :screenshotbot/figma/dropdown)

(named-readtables:in-readtable markup:syntax)

(markup:deftag figma-drop-down (&key script-name run screenshot)
  (let ((id (format nil "a~a" (random 10000000000)))
        (existing-figma (find-existing-figma-link :channel
                                                  (recorder-run-channel run)
                                                  :screenshot-name
                                                  (screenshot-name screenshot))))
    <li>
      <a href= "#" class= "dropdown-toggle" data-bs-toggle= "dropdown"
         data-bs-target= id
         aria-expanded= "false" >Figma</a>
      <ul class= "dropdown-menu" >
        ,(unless existing-figma
           <li>
             <a class= "dropdown-item" href= (nibble () (associate-figma :channel (recorder-run-channel run) :screenshot-name (screenshot-name screenshot)  :redirect script-name))
                > Link to Figma</a>
           </li>)

        ,(when existing-figma
           <li>
             <a class= "dropdown-item d-flex align-items-center" href= (figma-link-url existing-figma) target= "_blank" >
               <mdi name= "open_in_new" class= "me-2"/>
               <span>View in Figma</span>
             </a>
           </li>)
        ,(when existing-figma
           <li>
             <a class= "dropdown-item d-flex align-items-center" href= (nibble ()
                                                       (delete-figma existing-figma :redirect script-name)) >
               <mdi name= "delete" class= "me-2"/>
               <span>Delete Figma</span>
             </a>
           </li>)        
      </ul>
    </li>))


(defun delete-figma (existing-figma &key redirect)
  (confirmation-page
   :yes (nibble () (bknr.datastore:delete-object existing-figma)
          (hex:safe-redirect redirect))
   :no redirect
   <div>
     Are you sure you want to delete this association from Figma?
   </div>))

