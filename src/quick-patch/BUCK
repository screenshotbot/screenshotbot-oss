load("//tools:asd.bzl", "asd_system")
load("//tools:lisp.bzl", "lisp_test")

asd_system(
    name = "quick-patch",
    srcs=glob(["**/*"]),
)

lisp_test(
    name = "tests",
    srcs = [
        "test-impl.lisp",
    ],
    deps = [
        ":quick-patch",
        "//quicklisp:cl-mock",
        "//quicklisp:tmpdir",
        "//quicklisp:str",
        "//quicklisp:fiveam",
    ],
)
