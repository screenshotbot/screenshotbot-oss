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
   #:channel-search-bar))
(in-package :screenshotbot/dashboard/channel-search-bar)

(named-readtables:in-readtable markup:syntax)

(deftag channel-search-bar (children &key channel target)
  "The channel name as a heading, followed by a click-to-expand search
box that filters TARGET (a selector for the results container, see the
.search handler in runs.js).

CHILDREN are rendered after the search box, e.g. run tags."
  (let ((name (or (channel-name channel) "Unnamed channel")))
    <div class= "channel-search-bar" >
      <h4 class= "channel-search-bar-name text-truncate" title=name >
        ,(shortened-channel-name name :length 40)
      </h4>
      <div class= "channel-search-bar-search collapsed" >
        <div class= "input-group">
          <button type= "button" class= "input-group-text search-toggle"
                  title= "Search" aria-label= "Search" >
            <mdi name= "search" />
          </button>
          <input class= "form-control search d-inline-block" type= "text" autocomplete= "off"
                 placeholder= "Search..."
                 data-target=target />
        </div>
        ,@children
      </div>
    </div>))
