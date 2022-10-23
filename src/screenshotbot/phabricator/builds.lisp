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
  (:import-from #:util/phabricator/conduit
                #:phab-instance
                #:call-conduit)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/phabricator/builds)

(defclass build-info (store-object)
  ((diff :initarg :diff
         :reader build-info-diff)
   (revision :initarg :revision
             :reader build-info-revision)
   (target-phid :initarg :target-phid
                :reader target-phid)
   (build-phid :initarg :build-phid
               :reader build-phid)
   (company :initarg :company
            :index-type hash-index
            :index-reader build-infos-for-company)
   (ts :initarg :ts
       :reader ts))
  (:metaclass persistent-class)
  (:default-initargs :ts (get-universal-time)))

(defapi (nil :uri "/phabricator/update-build" :method :post)
        ((diff :parameter-type 'integer) (revision :parameter-type 'integer) target-phid
         build-phid)
  (make-instance 'build-info
                 :diff diff
                 :revision revision
                 :target-phid target-phid
                 :build-phid build-phid
                 :company (current-company))
  "OK")

(defmethod sendmessage ((phab phab-instance) (self build-info) type)
  (let ((type (str:downcase type)))
    (let ((args `(("buildTargetPHID" . ,(target-phid self))
                  ("type" . ,type))))
      (call-conduit
       phab
       "harbormaster.sendmessage"
       args))))

#|
(sendmessage *phab*
(car (reverse (bknr.datastore:class-instances 'build-info)))

:fail)
|#
