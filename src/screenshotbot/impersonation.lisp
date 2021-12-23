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
  (setf (current-user) user)
  (set-cookie (cookies self)
              "imp" "1"))

(defmethod impersonatedp ((self impersonation))
  (equal "1" (get-cookie (cookies self) "imp")))

(defmethod logout ((self impersonation))
  (set-cookie (cookies self) "imp" ""))
