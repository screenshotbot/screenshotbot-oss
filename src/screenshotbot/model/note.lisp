;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/note
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:screenshotbot/user-api
                #:user)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:note
   #:user
   #:message
   #:note-for
   #:find-notes-for))
(in-package :screenshotbot/model/note)

(defclass note (store-object)
  ((user :initarg :user
         :reader user)
   (message :initarg :message
            :reader message)
   (for :initarg :For
        :reader note-for
        :index-type hash-index
        :index-reader find-notes-for)
   (created-at :initform 0
               :initarg :created-at))
  (:metaclass persistent-class)
  (:default-initargs :created-at (get-universal-time)))
