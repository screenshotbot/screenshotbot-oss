(defpackage :screenshotbot/impersonation
  (:use #:cl)
  (:import-from #:clues/injectable
                #:injectable)
  (:import-from #:screenshotbot/user-api
                #:current-user
                #:user)
  (:import-from #:util/cookies
                #:cookies
                #:get-cookie
                #:set-cookie)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:impersonatedp
   #:impersonate
   #:impersonation
   #:logout))
(in-package :screenshotbot/impersonation)

(defclass impersonation ()
  ((cookies :inject cookies
            :reader cookies))
  (:metaclass injectable))

(defmethod impersonate ((self impersonation) (user user))
  (let ((admin-user (current-user)))
    (setf (current-user) user)
    (set-cookie (cookies self)
                "imp" "1")
    (setf (auth:session-value :admin-user)  admin-user)))

(defmethod impersonatedp ((self impersonation))
  (equal "1" (get-cookie (cookies self) "imp")))

(defmethod admin-user ((self impersonation))
  (auth:session-value :admin-user))

(defmethod logout ((self impersonation))
  (setf (auth:session-value :admin-user)  nil)
  (set-cookie (cookies self) "imp" ""))
