;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/dashboard/mask-builder
    (:use #:cl
          #:alexandria
          #:markup
          #:../model/screenshot
          #:nibble
          #:../model/image
          #:../model/channel
          #:../template)
  (:import-from #:../server
                #:with-login)
  (:import-from #:./run-page
                #:mask-editor))


(markup:enable-reader)

(defun mask-editor (channel screenshot &key (redirect "/"))
  (with-login ()
   (with-open-stream (s (open-image-stream (screenshot-image screenshot)))
     (let* ((im (opticl:read-png-stream s))
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
       (destructuring-bind (h w z) (array-dimensions im)
         (declare (ignore z))
         <app-template>
         <form action=save method= "POST" id= "mask-editor-form" >
         <h1>Edit Masks</h1>

         <p>
         Masks specifies areas of the image that won't count towards screenshot comparison. For instance, you may use this to mask out animations and timestamps. Draw rectangles in the image below to specify the masked area.
         </p>

         <p>
         Any modifications to the mask will only affect future runs. Existing runs and reports will not be affected.
         </p>

             <canvas id= "mask-editor" width= w height= h
                     data-rects= (json:encode-json-to-string (coerce mask 'vector )) />
             <img id= "mask-editor-image"
                  src=(image-public-url (screenshot-image screenshot))
                  style= "display:none" />
             <script src= "/assets/js/fabric.min.js" />

             <div class= "mt-3" >
               <input type= "hidden" name= "json" value= "" />
               <input type= "submit" id= "save-masks" class= "btn btn-primary" value = "Save Masks" />
               <a href= "#" id= "clear-masks" class= "btn btn-danger">Clear Masks</a>
             </div>
           </form>
         </app-template>)))))
