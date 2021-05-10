(defpackage :build-utils/jar-file
  (:use :cl
   :alexandria
   :asdf)
  (:export :java-library
           :jar-file))
(in-package :build-utils/jar-file)

(defclass java-library (asdf:system)
  ())

(defclass jar-file (asdf:static-file)
  ((asdf::type :initform "jar")))
