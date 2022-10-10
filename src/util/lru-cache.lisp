;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/lru-cache
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:lru-cache
   #:with-cache-file))
(in-package :util/lru-cache)

(defclass lru-cache ()
  ((dir :initarg :dir
        :reader dir))
  (:documentation "An LRU cache for items stored on disk"))

(def-easy-macro with-cache-file (&binding pathname cache file &fn fn)
  (assert file)
  (funcall fn (path:catfile (dir cache) file)))
