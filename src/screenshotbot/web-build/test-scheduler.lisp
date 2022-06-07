(defpackage :screenshotbot/pro/web-build/test-scheduler
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/pro/web-build/scheduler
                #:%run-now
                #:next-runtime
                #:maybe-run-project
                #:project
                #:start-ts)
  (:import-from #:screenshotbot/pro/web-build/project
                #:next-job-at
                #:web-project)
  (:import-from #:bknr.datastore
                #:delete-object
                #:with-transaction)
  (:import-from #:util/store
                #:with-test-store)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/pro/web-build/test-scheduler)


(util/fiveam:def-suite)

(test start-ts
  (is (equal 82174 (start-ts 980))))

(def-fixture state (&key (schedule-p t))
  (with-test-store ()
   (cl-mock:with-mocks ()
     (let ((project (make-instance 'web-project
                                    :schedule-p schedule-p
                                    :schedule-every 2)))
       (unwind-protect
            (progn (&body))
         (delete-object project))))))

(test maybe-run-project-on-project-without-ts
  (with-fixture state ()
    (cl-mock:answer
     (next-runtime now 0 2)
     45)
   (let ((now (* 10 24 3600)))
     (maybe-run-project project
                        :now now
                        :start-ts 0)
     (is (equal 45
                (next-job-at project))))))

(test next-runtime ()
  (is (equal 3600
             (next-runtime 3600 0 1)))
  (is (equal 3602
             (next-runtime 3600 2 1)))
  (is (equal 7202
             (next-runtime 3600 2 2))))

(test maybe-run-project-on-project-with-ts
  (with-fixture state ()
    (cl-mock:answer
     (next-runtime now 0 2)
      45)
    (cl-mock:answer
        (%run-now project)
      nil)
    (with-transaction ()
      (setf (next-job-at project) 0))
    (let ((now (* 10 24 3600)))
      (maybe-run-project project
                         :now now
                         :start-ts 0)
      (is (equal 45
                 (next-job-at project))))))

(test does-not-do-anything-if-schedule-p-not-set
  (with-fixture state (:schedule-p
                       nil)
    (cl-mock:if-called
     'next-runtime
     (lambda (&rest args)
       (error "should not be called")))
    (let ((now (* 10 24 3600)))
      (maybe-run-project project
                         :now now
                         :start-ts 0)
      (is (equal nil
                 (next-job-at project))))))
