(defpackage :screenshotbot/github/read-repos
  (:use #:cl)
  #+ (or ccl lispworks)
  (:import-from #:util/java
                #:java-syntax
                #:read-java-field
                #:java-list->list
                #:new-instance)
  (:import-from #:screenshotbot/github/access-checks
                #:github-repo-id
                #:github-client)
  (:import-from #:oidc/oidc
                #:access-token-str)
  #+ (or ccl lispworks)
  (:import-from #:util/java/java
                #:*bfalse*
                #:*btrue*
                #:java-equals)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:read-repo-list
   #:can-edit-repo))
(in-package :screenshotbot/github/read-repos)

#+ (or ccl lispworks)
(named-readtables:in-readtable java-syntax)

(defun can-edit-repo (access-token repo)
  #- (or ccl lispworks)
  t
  #+ (or ccl lispworks)
  (let ((collab-service
          (new-instance #,org.eclipse.egit.github.core.service.CollaboratorService
                        (github-client :oauth-token
                                       (access-token-str access-token)))))
    (let ((collabp (#_isCollaborator
                    collab-service
                    (github-repo-id repo)
                    "tdrhq")))
      #+nil
      (error "collaborators are : ~s"
             (loop for user in
                         (java-list->list (#_getCollaborators
                                           collab-service
                                           (github-repo-id repo)))
                   collect
                   (#_getLogin user)))
      (equal "true" (#_toString collabp)))))
