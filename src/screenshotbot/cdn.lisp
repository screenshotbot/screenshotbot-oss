;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop/package:define-package :screenshotbot/cdn
    (:use #:cl)
  (:export #:script #:link #:img
           #:make-image-cdn-url))
(in-package :screenshotbot/cdn)

(markup:enable-reader)

(markup:deftag script (children &key src type)
  <:script src= (when src (util.cdn:make-cdn src)) type=type >,@ (progn children)</:script>)

(markup:deftag link (&key rel as href type crossorigin media id)
  <:link rel=rel as=as type=type crossorigin=crossorigin media=media
  id=id
  href= (util.cdn:make-cdn href) />)

(markup:deftag img (&key src (alt "Image") srcset class style height width id loading)
  (let ((util.cdn:*cdn-cache-key* "images4" ))
    <:img src= (if (str:starts-with-p "/image/blob/" (make-image-cdn-url src)) src (util.cdn:make-cdn src)) alt=alt srcset=srcset class=class style=style
          height=height width=width id=id
          loading=loading
    />))

(defun make-image-cdn-url (url)
  "This is a specific CDN to use for actual screenshot images. For now
  we're using our default CDN, but we might change this in the
  future. For instance, separate installations might have a different
  CDN."
  url)
