(load "~/quicklisp/setup.lisp")

(push #P "./" asdf:*central-registry*)

(ql:quickload :file-lock/tests)

(unless (fiveam:run-all-tests)
  (uiop:quit 1))

(uiop:quit 0)
