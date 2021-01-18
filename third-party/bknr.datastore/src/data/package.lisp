(in-package :cl-user)

(defpackage :bknr.datastore
  (:use :closer-common-lisp
        :bknr.utils :cl-interpol :cl-ppcre
        :bknr.indices :bknr.statistics
        :alexandria)
  #+cmu
  (:shadowing-import-from :common-lisp #:subtypep #:typep)
  (:shadowing-import-from :cl-interpol quote-meta-chars)
  (:shadowing-import-from :bknr.indices array-index)
  #|  (:shadow :cl #:get-internal-run-time #:get-internal-real-time #:get-universal-time #:sleep) |#
  (:export #:*store-debug*
           #:*store*
           #+(or) #:with-store ; currently not exported, does not work with indices

           #|
           ;; COMMON-LISP overloads to make sure that transaction
           ;; don't access time information
           #:get-internal-run-time
           #:get-internal-real-time
           #:get-universal-time
           #:sleep
           |#

           ;; store
           #:store
           #:mp-store
           #:store-guard
           #:store-state
           #:open-store
           #:close-store

           ;; transaction
           #:transaction
           #:transaction-function-symbol
           #:transaction-args
           #:transaction-timestamp
           #:current-transaction-timestamp
           #:in-transaction-p
           #:deftransaction

           ;; store-object
           #:persistent-class
           #:persistent-xml-class
           #:persistent-xml-class-importer
           #:define-persistent-class
           #:define-persistent-xml-class
           #:defpersistent-class

           #:store-object
           #:store-object-store
           #:store-object-id
           #:store-object-last-change
           #:store-object-touch
           #:print-store-object

           #:delete-object
           #:delete-objects
           #:cascade-delete-p
           #:cascading-delete-object

           #:initialize-transient-instance

           #:store-object-with-id
           #:store-objects-with-class
           #:class-instances            ; convenience alias
           #:store-objects-of-class
           #:all-store-objects
           #:map-store-objects
           #:prepare-for-snapshot
           #:find-store-object
           #:create-object-transaction
           #:tx-change-slot-values
           #:change-slot-values
           #:store-object-add-keywords
           #:store-object-remove-keywords
           #:store-object-set-keywords

           #:convert-slot-value-while-restoring

           #:persistent-change-class

           #:map-class-instances

           #:store-object-add-keywords
           #:store-object-remove-keywords
           #:store-object-set-keywords

           ;; operations
           #:execute
           #:restore
           #:snapshot
           #:with-store-guard
           #:with-transaction
           #:store-objects
           #:store-stats

           #:blob
           #:blob-type
           #:blob-mime-type
           #:blob-timestamp
           #:blob-pathname
           #:with-open-blob
           #:blob-size
           #:blob-to-stream
           #:blob-to-file
           #:blob-from-stream
           #:blob-from-string
           #:blob-from-file
           #:blob-from-array
           #:make-blob-from-file
           #:rename-file-to-blob
           #:store-blob-root-tempdir

           #:find-refs

           ;; Subsystems and subsystem API
           #:store-object-subsystem
           #:blob-subsystem

           #:initialize-subsystem
           #:snapshot-subsystem
           #:restore-subsystem
           #:ensure-store-current-directory

           ;; JSON serialization
           #:with-json-ignore-slots
           #:*json-ignore-slots*))

(defpackage :bknr.datastore.tests
  (:use :cl :bknr.datastore :bknr.indices :unit-test))