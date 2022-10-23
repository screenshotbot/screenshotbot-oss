(defpackage :screenshotbot/phabricator/builds
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/api/core
                #:defapi)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/user-api
                #:current-company)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/phabricator/builds)

(defclass build-info (store-object)
  ((diff :initarg :diff
         :reader build-info-diff)
   (revision :initarg :revision
             :reader build-info-revision)
   (target-phid :initarg :target-phid
                :reader target-phid)
   (company :initarg :company
            :index-type hash-index
            :index-reader build-infos-for-company)
   (ts :initarg :ts
       :reader ts))
  (:metaclass persistent-class)
  (:default-initargs :ts (get-universal-time)))

(defapi (nil :uri "/phabricator/update-build" :method :post)
        ((diff :parameter-type 'integer) (revision :parameter-type 'integer) target-phid)
  (make-instance 'build-info
                 :diff diff
                 :revision revision
                 :target-phid target-phid
                 :company (current-company))
  "OK")
