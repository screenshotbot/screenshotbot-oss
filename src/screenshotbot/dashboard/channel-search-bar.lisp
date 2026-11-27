;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/channel-search-bar
  (:use #:cl)
  (:import-from #:markup
                #:deftag)
  (:import-from #:screenshotbot/template
                #:mdi)
  (:import-from #:screenshotbot/model/channel
                #:shortened-channel-name)
  (:import-from #:screenshotbot/user-api
                #:channel-name)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:channel-search-bar
   #:search-box))
(in-package :screenshotbot/dashboard/channel-search-bar)

(named-readtables:in-readtable markup:syntax)

(deftag search-box (children &key target small-p)
  "A search button that expands into a search box when clicked, and
filters TARGET (a selector for the results container, see the .search
handler in runs.js). It collapses again when it's empty and loses
focus.

If SMALL-P, it's sized to sit next to .btn-sm buttons.

CHILDREN are rendered after the search box."
  <div class= "channel-search-bar-search collapsed" >
    <div class= (if small-p "input-group input-group-sm" "input-group") >
      <button type= "button" class= "input-group-text search-toggle"
              title= "Search" aria-label= "Search" >
        <mdi name= "search" />
      </button>
      <input class= "form-control search d-inline-block" type= "text" autocomplete= "off"
             placeholder= "Search..."
             data-target=target />
    </div>
    ,@children
  </div>)

(deftag channel-search-bar (children &key channel target (search-p t))
  "The channel name as a heading, followed by a SEARCH-BOX that filters
TARGET.

If SEARCH-P is NIL, the search box is left out, e.g. because the page
renders its own SEARCH-BOX somewhere else.

CHILDREN are rendered after the search box, e.g. run tags."
  (let ((name (or (channel-name channel) "Unnamed channel")))
    <div class= "channel-search-bar" >
      <h4 class= "channel-search-bar-name text-truncate" title=name >
        ,(shortened-channel-name name :length 40)
      </h4>
      ,(cond
         (search-p
          <search-box target=target >
            ,@children
          </search-box>)
         (t
          (markup:make-merge-tag children)))
    </div>))
