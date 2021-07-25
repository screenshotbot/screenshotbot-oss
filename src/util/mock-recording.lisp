(pkg:define-package :util/mock-recording
    (:use #:cl
          #:alexandria)
  (:import-from #:cl-mock
                #:with-mocks
                #:call-previous
                #:if-called)
  (:export
   #:with-recording))

(defun call-with-recording (mocked-function file fn &key record skip-args)
  (flet ((remove-skip-args (args)
           (loop for x in args
                 for i from 0 to 10000000
                 if (not (member i skip-args))
                   collect x)))
   (cond
     (record
      (let ((recording))
        (with-mocks ()
          (if-called mocked-function
                     (lambda (&rest args)
                       (let ((res
                               (call-previous)))
                         (push (cons (remove-skip-args args) res)
                               recording)
                         res)))
          (let ((res (funcall fn)))
            (with-open-file (stream file :direction :output
                                         :if-exists :supersede)
              (write (reverse recording) :stream stream))
            res))))
     (t
      (let ((recording (read-from-string (uiop:read-file-string file))))
        (with-mocks ()
          (if-called mocked-function
                     (lambda (&rest args)
                       (let ((next (pop recording)))
                         (unless (equal (car next) (remove-skip-args args))
                           (error "Next args in recording is ~S but got ~S"
                                  (car next) args))
                         (cdr next))))
          (funcall fn)))))))

(defmacro with-recording ((mocked-function file &key record skip-args) &body body)
  `(call-with-recording ',mocked-function ,file
                        (lambda () ,@body)
                        :record ,record
                        :skip-args ,skip-args))
