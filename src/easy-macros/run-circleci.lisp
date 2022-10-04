(load "~/quicklisp/setup.lisp")

(push #P "./" asdf:*central-registry*)

(ql:quickload :quick-patch)
(quick-patch:register "https://github.com/tdrhq/fiveam-matchers.git"
                      "master")
(quick-patch:checkout-all "build/quick-patch-oss/")

(ql:quickload :easy-macros/tests)

(unless (fiveam:run-all-tests)
  (uiop:quit 1))

(uiop:quit 0)
