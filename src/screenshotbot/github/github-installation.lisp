;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/github/github-installation
    (:use #:cl
          #:alexandria)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:unique-index
                #:store-object)
  (:import-from #:../model/company
                #:installation-id)
  (:export #:github-installation
           #:github-installation-with-repo-name
           #:installation-id))

(defclass github-installation (store-object)
  ((repo-name
    :initarg :repo-name
    :index-type unique-index
    :index-initargs (:test #'equal)
    :index-reader github-installation-with-repo-name)
   (installation-id
    :initarg :installation-id
    :accessor installation-id))
  (:metaclass persistent-class))
