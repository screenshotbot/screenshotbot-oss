(uiop:define-package :screenshotbot/java
    (:use-reexport
     #+ (or ccl lispworks)
     :screenshotbot/java/java
     :screenshotbot/java/reader))
