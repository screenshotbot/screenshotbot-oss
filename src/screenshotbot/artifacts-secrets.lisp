(pkg:define-package :screenshotbot/artifacts-secrets
    (:use #:cl
          #:alexandria)
  (:import-from #:screenshotbot/secret
                #:defsecret))

(defsecret :artifact-upload-key
  "Key used to authenticate that the artifact upload is coming from a
  trusted source")
