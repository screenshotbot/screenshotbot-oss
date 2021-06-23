;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/settings/settings-template
    (:use #:cl
          #:alexandria
          #:markup)
  (:import-from #:../server
                #:defhandler)
  (:import-from #:../template
                #:dashboard-template)
  (:import-from #:../installation
                #:installation)
  (:import-from #:../api/core
                #:defapi)
  (:export #:settings-template
           #:defapi)
  (:use-reexport #:../settings-api))

(markup:enable-reader)

(deftag settings-menu-item (children &key href)
  <li class= "settings-nav-item" ><a href= href >,@children </a></li>)

(defun settings-list-sections ()
  (remove-duplicates
   (loop for (nil . x) in (reverse (all-settings (installation)))
         collect (settings-section x))))

(deftag settings-render-section (&key title
                                 section)
  <markup:merge-tag>
  <li class= "settings-nav-title settings-nav-item">,(progn title)</li>
  ,@ (loop for (nil . x)  in (reverse (all-settings (installation)))
           if (eql (settings-section x) section)
             collect <settings-menu-item href= (format nil "/settings/~a" (settings-name x))>,(settings-title x)</settings-menu-item>)
  </markup:merge-tag>)

(deftag settings-template (children)
  (let ((sections (mapcar 'settings-section (mapcar 'cdr (all-settings (installation))))))
    (assert (equal (sort `(nil :vcs :tasks) #'string<) (sort (remove-duplicates sections) #'string<)))

      <dashboard-template scripts= (list
                                "/assets/js/settings.js")
                      >

    <div class= "row" style= "min-height: 100vh" >
      <div class= "col-md-2 border-right settings-nav-bar">
        <ul class= "settings-nav" >
          <settings-render-section title= "Account" section=nil />
          <settings-render-section title= "VCS" section=:vcs />
          <settings-render-section title= "Tasks" section=:tasks />
        </ul>
      </div>

      <div class= "col-md-10">
        <div class= "settings-content">
          ,@children
        </div>
      </div>
    </div>
  </dashboard-template>))

(defhandler (test-settings :uri "/settings") ()
  <settings-template>
    content
  </settings-template>)


(defhandler (render-single-settings-page :uri "/settings/:name"
                                         :method :get
                                         :want-login t) (name)
  (let ((installation (installation)))
   (loop for (nil . x) in (all-settings installation)
         if (string= (settings-name x)
                     name)
           do (return
                (funcall (settings-handler x)))
         finally
            (error "Could not find settings page ~a" name))))
