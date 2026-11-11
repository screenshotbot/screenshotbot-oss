(in-package :sentry-client)

(defclass async-sentry-client (sentry-client)
  ())

(defvar *tasks-runner* nil)
(defvar *tasks-thread* nil)

(defun maybe-initialize-tasks ()
  (when (null *tasks-runner*)
    (setf *tasks-runner* (make-instance 'simple-tasks:queued-runner))
    (setf *tasks-thread* (simple-tasks:make-runner-thread *tasks-runner*))))

(defun initialize-async-sentry-client (dsn &rest args &key tags)
  (setf *sentry-client*
        (make-instance 'async-sentry-client
                       :dsn (read-dsn dsn)
                       :tags tags)))

(defmethod initialize-instance :after ((sentry-client async-sentry-client) &rest initargs)
  (maybe-initialize-tasks))

(defmethod client-capture-exception ((sentry-client async-sentry-client) condition &rest args &key tags)
  (let ((json (encode-exception-event condition sentry-client)))
    (let ((task (make-instance 'simple-tasks:call-task
                             :func (lambda () (post-sentry-request json sentry-client)))))
      (simple-tasks:schedule-task task *tasks-runner*))))
