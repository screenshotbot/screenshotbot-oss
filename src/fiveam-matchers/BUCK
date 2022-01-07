load("//tools:lisp.bzl", "lisp_library", "lisp_test")

lisp_library(
    name = "fiveam-matchers",
    srcs = [
        "core.lisp",
        "lists.lisp",
        "has-length.lisp",
        "every-item.lisp",
        "all.lisp",
    ],
    deps = [
        "//quicklisp:fiveam",
    ],
    visibility = [
        "PUBLIC",
    ],
)

lisp_test(
    name="tests",
    srcs = [
        "test-core.lisp",
        "test-lists.lisp",
    ],
    deps = [
        ":fiveam-matchers",
        "//src/util:fiveam",
    ],
)
