(pkg:define-package :screenshotbot/test-store
  (:use #:cl
        #:alexandria
        #:bknr.datastore
        #:util
        #:fiveam
        #:./model/recorder-run
        #:./model/core))

(defvar *tmpdir*)

(defun reset-store (&optional why)
  (declare (optimize (speed 0) (debug 3)))
  (log:info "Reset store: ~a" why)
  (when *store*
    (bknr.datastore::close-store-object *store*)
    (assert (not (equal "object-store"
                        (car (last (pathname-directory (bknr.datastore::store-directory *store*)))))))
    (close-store)
    (hcl:gc-generation 0))
  (make-instance 'util:safe-mp-store
                 :directory *tmpdir*
                 :subsystems (util::store-subsystems)))

(def-fixture state ()
  (tmpdir:with-tmpdir (s)
    (let ((*tmpdir* s)
          (*store* nil))
      (reset-store "initial")
      (&body))))

(test simple-creation
  (with-fixture state ()
    (let* ((run (make-instance 'recorder-run))
           (id (store-object-id run))
           (promo-log (promotion-log run))
           (old-ts (%created-at run))
           (old-oid (oid run)))
      (log:info "Before snapshot")
      (snapshot)
      (log:info "After snapshot")
      (sleep 2)
      (reset-store "after snapshot")
      (log:info "After reset")
      (let ((updated-run (store-object-with-id id)))
        (is (typep updated-run 'recorder-run))
        (is (not (eql run updated-run)))
        (is (not (eql promo-log (promotion-log updated-run))))
        (is (eql
             (%created-at run)
             (%created-at updated-run)))
        (is (equal old-oid
                   (oid updated-run)))))))

(test re-creation-without-snapshot
  (with-fixture state ()
    (let* ((run (make-instance 'recorder-run))
           (id (store-object-id run))
           (promo-log (promotion-log run))
           (old-ts (%created-at run))
           (old-oid (oid run)))
      (is-true (%created-at run))
      (sleep 1)
      (reset-store "after snapshot")
      (log:info "After reset")
      (let ((updated-run (store-object-with-id id)))
        (is (typep updated-run 'recorder-run))
        (is (not (eql run updated-run)))
        (is (not (eql promo-log (promotion-log updated-run))))
        (is (eql
             (%created-at run)
             (%created-at updated-run)))
        (is (equal
             old-oid
             (oid updated-run)))))))
