"""Bazel rules for building Nim programs."""

def nim_binary(name, main, deps = [], **kwargs):
    """Compiles a Nim source file into an executable binary.

    Args:
        name: The name of the target (also used as the output binary name)
        main: The main .nim source file to compile
        deps: List of additional .nim files that the main file depends on
        **kwargs: Additional arguments passed to sh_binary (e.g., visibility)
    """
    bin_name = name + "_bin"

    native.genrule(
        name = bin_name,
        srcs = [main] + deps,
        outs = [name + "_exe"],
        cmd = "nim compile --nimcache:$(@D)/nimcache --out:$@ $(location " + main + ")",
        executable = True,
    )

    native.sh_binary(
        name = name,
        srcs = [":" + bin_name],
        **kwargs
    )

def nim_test(name, main, deps = [], **kwargs):
    """Compiles and runs a Nim test file.

    Args:
        name: The name of the test target
        main: The main .nim test file to compile and run
        deps: List of additional .nim files that the test file depends on
        **kwargs: Additional arguments passed to sh_test (e.g., size, timeout)
    """
    bin_name = name + "_bin"

    native.genrule(
        name = bin_name,
        srcs = [main] + deps,
        outs = [name + "_exe"],
        cmd = "nim compile --nimcache:$(@D)/nimcache -r --out:$@ $(location " + main + ")",
        executable = True,
    )

    native.sh_test(
        name = name,
        srcs = [":" + bin_name],
        **kwargs
    )
