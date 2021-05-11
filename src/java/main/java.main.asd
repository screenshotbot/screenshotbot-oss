(defpackage :java.libs-asdf
  (:use :cl :asdf))
(in-package :java.libs-asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
   (unless (find-package :build-utils)
     (asdf:operate 'asdf:load-op :build-utils)
     (use-package :build-utils)))

(defsystem java.main
  :class build-utils:java-library
  :defsystem-depends-on (:build-utils)
  :depends-on (:java.libs)
  :components ((jar-file "lispcalls")
               (:module "com/atlassian/oauth/client/example"
                :components ((build-utils:java-file "ClientMain")
                             (build-utils:java-file "JiraOAuthClient")
                             (build-utils:java-file "JiraOAuthGetTemporaryToken")
                             (build-utils:java-file "OAuthAuthenticationHandler")
                             (build-utils:java-file "PropertiesClient")
                             (build-utils:java-file "Command")
                             (build-utils:java-file "JiraOAuthGetAccessToken")
                             (build-utils:java-file "JiraOAuthTokenFactory")
                             (build-utils:java-file "OAuthClient")))
               (:module "com/tdrhq"
                :components (#-screenshotbot-oss
                             (build-utils:java-file "CustomCommitService")
                             #-screenshotbot-oss
                             (build-utils:java-file "Github")
                             #-screenshotbot-oss
                             (build-utils:java-file "PemUtils")
                             (build-utils:java-file "PrimitiveWrapper")
                             (build-utils:java-file "SimpleNativeLibrary")
                             #-screenshotbot-oss
                             (build-utils:java-file "TdrhqS3")
                             (build-utils:java-file "Whitebox")))))
