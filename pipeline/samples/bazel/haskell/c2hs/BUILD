load("@io_tweag_rules_haskell//haskell:c2hs.bzl", "c2hs_library")
load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

cc_library(
    name = "somelib",
    hdrs = ["cbits/somelib.h"],
    strip_include_prefix = "cbits",
)

c2hs_library(
    name = "foo",
    srcs = ["src/Foo/Foo.chs"],
    src_strip_prefix = "src",
    deps = [":somelib"],
)

c2hs_library(
    name = "bar",
    srcs = ["Bar.chs"],
    deps = [":foo"],
)
