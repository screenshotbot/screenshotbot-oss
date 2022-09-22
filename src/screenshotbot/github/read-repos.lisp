(defpackage :screenshotbot/github/read-repos
  (:use #:cl)
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
  (:import-from #:util/java/java
                #:*bfalse*
                #:*btrue*
                #:java-equals)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:read-repo-list
   #:can-edit-repo))
(in-package :screenshotbot/github/read-repos)

(named-readtables:in-readtable java-syntax)

(defun read-repo-list (access-token)
  (let ((repo-service
          (new-instance #,org.eclipse.egit.github.core.service.RepositoryService
                        (github-client :oauth-token
                                       (access-token-str access-token)))))
    (let ((repos (java-list->list
                  (#_getRepositories repo-service))))
      (loop for repo in repos
            collect
            (format nil "~a/~a"
                    (#_getLogin (#_getOwner repo))
                    (#_getName repo))))))

(defun can-edit-repo (access-token repo)
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
