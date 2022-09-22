(defpackage :screenshotbot/github/read-repos
  (:use #:cl)
  (:import-from #:util/java
                #:java-syntax
                #:read-java-field
                #:java-list->list
                #:new-instance)
  (:import-from #:screenshotbot/github/access-checks
                #:github-client)
  (:import-from #:oidc/oidc
                #:access-token-str)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:read-repo-list))
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
