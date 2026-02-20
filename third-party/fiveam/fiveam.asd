;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; -*-

#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
    (error "You need ASDF >= 3.1 to load this system correctly."))

(defsystem :fiveam
  :author "Edward Marco Baringer <mb@bese.it>"
  :version (:read-file-form "version.sexp")
  :description "A simple regression testing framework"
  :license "BSD"
  :depends-on (:alexandria :net.didierverna.asdf-flv  :trivial-backtrace)
  :pathname "src/"
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "check" :depends-on ("package" "utils"))
               (:file "fixture" :depends-on ("package"))
               (:file "classes" :depends-on ("package"))
               (:file "random" :depends-on ("package" "check"))
               (:file "test" :depends-on ("package" "fixture" "classes"))
               (:file "explain" :depends-on ("package" "utils" "check" "classes" "random"))
               (:file "suite" :depends-on ("package" "test" "classes"))
               (:file "run" :depends-on ("package" "check" "classes" "test" "explain" "suite")))
  :in-order-to ((test-op (test-op :fiveam/test))))

(defsystem :fiveam/test
  :author "Edward Marco Baringer <mb@bese.it>"
  :description "FiveAM's own test suite"
  :license "BSD"
  :depends-on (:fiveam)
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :5am :run! :it.bese.fiveam)))

;;;;@include "src/package.lisp"

;;;;@include "t/example.lisp"
