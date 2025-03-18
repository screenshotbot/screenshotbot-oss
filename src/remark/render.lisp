;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :remark/render
  (:use #:cl)
  (:import-from #:remark/nodes
                #:page-default-p
                #:page-name
                #:locate-page-by-alias
                #:only-show-on-staging?
                #:toplevel-landing-page
                #:*toplevel*
                #:page-generator
                #:locate-page
                #:section-id
                #:toplevel-prefix
                #:find-node
                #:section-title
                #:toplevel
                #:section
                #:section-children)
  (:import-from #:remark/markdown
                #:header-id)
  (:import-from #:util/events
                #:push-event)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:remark/search
                #:search-remarks)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:render-table-of-contents
   #:render-page))
(in-package :remark/render)

(markup:enable-reader)


(defmethod link-to (page &key (toplevel *toplevel*))
  (labels ((%find (node prefix)
             (when (eql node page)
               (return-from link-to prefix))
             (when (typep node 'section)
              (loop for child-name in (section-children node)
                    for child = (find-node child-name)
                    do
                       (%find child (format nil "~a/~a" prefix (section-id child)))))))
    (%find toplevel (toplevel-prefix toplevel))
    :not-found))

(defun render-page-name (page)
  (let ((*package* nil)) (format nil "~s" (page-name page))))

(defmethod render-table-of-contents ((section section) prefix current-page
                                     &key staging?)
  <ul>
    ,@ (loop for child-name in (section-children section)
             for child = (find-node child-name)
             for activep = (eql child current-page)
             if (or staging?
                    (not (only-show-on-staging? child)))
             collect
             <li>
               ,(cond
                  ((typep child 'section)
                   <markup:merge-tag>
                     <span class= "section-title">,(section-title child)</span>
                     ,(render-table-of-contents child (format nil "~a/~a" prefix (section-id child)) current-page)
                   </markup:merge-tag>)
                  (t
                   <span class= "pagep-title" data-page-name= (render-page-name child) > <a href= (format nil "~a/~a" prefix (section-id  child)) class= (when activep "active") >,(section-title child)</a></span>
))
             </li>)
  </ul>)


(defmethod render-outline (page)
  <div class= "remark-outline">
    <div class= "content">
      <h4>Outline</h4>
      <ul>
        ,@(let ((ret))
            (mquery:with-document (page)
              (loop for item in (mquery:$ "h2")
                    for title = (or
                                 (mquery:attr item "title")
                                 (car (markup:xml-tag-children item)))
                    do
                       (push <li><a href= (format nil "#~a" (header-id title)) >,(progn title)</a> </li> ret)))
            (nreverse ret))
      </ul>
    </div>
  </div>)

(defmethod page-generator ((section section))
  (lambda ()
    <div>
      <h3>In this section</h3>

      <ul>
        ,@ (loop for page in (section-children section)
                 collect
                 <li><a href= (link-to (find-node page)) >,(section-title (find-node page))</a></li>)
      </ul>
    </div>))

(defun %search (toplevel query)
  (let ((results (search-remarks toplevel query)))
    (cond
      ((not results)
       "No search results. Contact us at support@screenshotbot.io for help!")
      (t
       <div>
         <h3>Search Results</h3>
           ,@ (loop for result in results
                    collect
                    <div>
                      <a href= (link-to result :toplevel toplevel) >,(section-title result)</a>
                    </div>)
       </div>))))

(defmethod render-page ((toplevel toplevel) script &key staging?)
  (let ((*toplevel* toplevel))
   (multiple-value-bind (page section)
       (locate-page toplevel script)

     (unless page
       (let ((page (locate-page-by-alias toplevel script)))
         (when page
           (hex:safe-redirect
            (link-to page :toplevel toplevel))))
       (push-event :docs.404 :script script))
     (let* ((page-content (when page
                            (funcall (page-generator page)))))
       <div class= "remarks" >
         <div class= "remark-toc">
           <div class= "remark-toc-dropdown">
             <button type= "button" data-bs-toggle= "collapse"
                     data-bs-target= ".remark-toc .content"
                     class= "navbar-toggler" >
               <span class= "navbar-toggler-icon" />
             </button>
             ,(cond
                (section (section-title section))
                (t "Documentation"))
           </div>
           <div class= "content">
             ,(unless (hunchentoot:parameter "search")
                <div class="input-group doc-search">
                  <input class="form-control border-end-0 border" type="search" placeholder="Search..." id="example-search-input" name= "query" data-search= (nibble (query) (%search toplevel query)) />
                  <span class="input-group-append">
                    <button class="btn btn-outline-secondary bg-white border-start-0 border ms-n5" type="button" >
                      <svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 -960 960 960" width="24"><path d="M784-120 532-372q-30 24-69 38t-83 14q-109 0-184.5-75.5T120-580q0-109 75.5-184.5T380-840q109 0 184.5 75.5T640-580q0 44-14 83t-38 69l252 252-56 56ZM380-400q75 0 127.5-52.5T560-580q0-75-52.5-127.5T380-760q-75 0-127.5 52.5T200-580q0 75 52.5 127.5T380-400Z"/></svg>
                    </button>
                  </span>
                </div>)
             ,(render-table-of-contents toplevel (toplevel-prefix toplevel) page
                                        :staging? staging?)
           </div>
         </div>

       ,(when page
          <div class= "remark-body">
            <div class= "search-results " style= "display:none" >
            </div>
            <div class= "actual-content">
              ,(progn page-content)
            </div>
          </div>)

       ,(when page
          (render-outline page-content))
       </div>))))
