(defpackage :util/fake-mp
  (:use #:cl)
  (:import-from #:util/misc
                #:or-setf)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:process-p
   #:process-wait-for-event
   #:process-send
   #:process-mailbox))
(in-package :util/fake-mp)


(defvar *mailboxes* (trivial-garbage:make-weak-hash-table
                     :weakness :key))

(defun process-mailbox (process)
  (gethash process *mailboxes*))

(defun (setf process-mailbox) (mailbox process)
  (setf (gethash process *mailboxes*) mailbox))

(defun ensure-mailbox (process)
  (or-setf
   (process-mailbox process)
   (mailbox:make-mailbox)
   :thread-safe t))

(defun process-p (process)
  (bt:threadp process))

(defun process-wait-for-event (&key no-hang-p)
  (let ((mailbox (ensure-mailbox (bt:current-thread))))
    (assert no-hang-p)
    (mailbox:read-mail mailbox)))

(defun process-send (process message)
  (let ((mailbox (ensure-mailbox process)))
    (mailbox:post-mail message mailbox)))
