(load "~/quicklisp/setup.lisp")

(push #P "./" asdf:*central-registry*)

(ql:quickload :quick-patch/tests)

(unless (fiveam:run-all-tests)
  (uiop:quit 1))

(uiop:quit 0)
