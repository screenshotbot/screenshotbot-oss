;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/dashboard/mask-builder
  (:use #:cl
        #:alexandria
        #:markup
        #:screenshotbot/model/screenshot
        #:nibble
        #:screenshotbot/model/image
        #:screenshotbot/model/channel
        #:screenshotbot/template)
  (:import-from #:screenshotbot/server
                #:with-login)
  (:import-from #:screenshotbot/dashboard/run-page
                #:mask-editor)
  (:import-from #:screenshotbot/model/image
                #:dimension-height
                #:dimension-width
                #:image-dimensions)
  (:import-from #:core/ui/mdi
                #:mdi))
(in-package :screenshotbot/dashboard/mask-builder)


(markup:enable-reader)

(defun mask-editor (channel screenshot &key (redirect "/")
                                         overlay)
  (with-login ()
   (let* ((dim (image-dimensions screenshot))
          (mask (assoc-value (masks channel) (screenshot-name screenshot) :test 'equal))
          (save (nibble (json :method :post)
                  (let ((new-mask (loop for x in (json:decode-json-from-string json)
                                        collect
                                        (apply 'make-instance
                                                'mask-rect
                                                 :allow-other-keys t
                                                 (alist-plist x)))))
                    (set-channel-screenshot-mask
                     channel
                     (screenshot-name screenshot)
                     new-mask))
                  (hex:safe-redirect redirect))))
     <app-template>
       <form action=save method= "POST" id= "mask-editor-form" >
         <div class= "page-title-box main-content" >
           <h4 class= "page-title" >Edit Masks</h4>

           <div class= "float-end" >
             <input type= "hidden" name= "json" value= "" />
             <a href= "#" id= "clear-masks" class= "btn btn-danger btn-sm">
               <mdi name= "delete_sweep" />
               Clear All
             </a>
             <a href= "javascript:window.history.back()" class= "btn btn-sm btn-secondary">
               Discard Changes
             </a>
             <input type= "submit" id= "save-masks" class= "btn btn-primary btn-sm" value = "Save Masks" />

           </div>
         </div>
         <div class= "main-content pb-3" >

           <div class= "info-box text-muted" style= "max-width: 50em" >
             <p class= "mt-3 text-muted" >
               Masks specifies areas of the image that won't count towards screenshot comparison. For instance, you may use this to mask out animations and timestamps. Draw rectangles in the image below to specify the masked area.
             </p>

             <p>
               Any modifications to the mask will only affect future runs. Existing runs and reports will not be affected.
             </p>
           </div>



           <div class= "mt-3 mb-3" >
           </div>

           <canvas id= "mask-editor" width= (dimension-width dim) height= (dimension-height dim)
                   data-rects= (json:encode-json-to-string (coerce mask 'vector )) />
           <img id= "mask-editor-image"
                src=(image-public-url (screenshot-image screenshot))
                style= "display:none" />
           ,(when overlay
              <img id= "mask-editor-overlay"
                   src= (image-public-url overlay)
                   style= "display:none" />)
           <script src= "/assets/js/fabric.min.js" />
         </div>
       </form>
     </app-template>)))
