"""Bazel rules for building Haskell programs."""

def haskell_binary(name, main, deps = [], ghc_flags = ["-O2"], **kwargs):
    """Compiles a Haskell source file into an executable binary.

    Args:
        name: The name of the target
        main: The main .hs source file to compile
        deps: List of additional .hs files that the main file depends on
        ghc_flags: List of GHC compiler flags (default: ["-O2"])
        **kwargs: Additional arguments passed to genrule (e.g., visibility, executable)
    """
    flags_str = " ".join(ghc_flags)

    native.genrule(
        name = name,
        srcs = [main] + deps,
        outs = [name + "_exe"],
        cmd = """
            cd $$(dirname $(location {main}))
            ghc {flags} \\
                -outputdir $$OLDPWD/$(@D) \\
                -o $$OLDPWD/$@ \\
                {main_basename}
        """.format(
            main = main,
            flags = flags_str,
            main_basename = main.split("/")[-1],
        ),
        executable = True,
        **kwargs
    )

def haskell_test(name, main, deps = [], ghc_flags = ["-O2"], **kwargs):
    """Compiles and runs a Haskell test file.

    Args:
        name: The name of the test target
        main: The main .hs test file to compile
        deps: List of additional .hs files that the test file depends on
        ghc_flags: List of GHC compiler flags (default: ["-O2"])
        **kwargs: Additional arguments passed to sh_test (e.g., size, timeout)
    """
    bin_name = name + "_bin"
    flags_str = " ".join(ghc_flags)

    native.genrule(
        name = bin_name,
        srcs = [main] + deps,
        outs = [name + "_exe"],
        cmd = """
            cd $$(dirname $(location {main}))
            ghc {flags} \\
                -outputdir $$OLDPWD/$(@D) \\
                -o $$OLDPWD/$@ \\
                {main_basename}
        """.format(
            main = main,
            flags = flags_str,
            main_basename = main.split("/")[-1],
        ),
        executable = True,
    )

    native.sh_test(
        name = name,
        srcs = [":" + bin_name],
        **kwargs
    )
