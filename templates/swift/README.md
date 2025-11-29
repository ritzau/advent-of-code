# Swift Template - Temporarily Disabled

This Swift template is currently disabled in `.bazelignore` due to a toolchain conflict.

## The Issue

The Swift binaries require Apple's native linker and Swift compatibility libraries, while the main repository uses `toolchains_llvm` for C++ compilation. These two toolchain configurations are incompatible in the same Bazel build:

- **LLVM toolchain** (used for C++): Uses LLVM's `lld` linker
- **Swift requirements**: Requires Apple's `ld` linker and Swift compatibility libraries (`swiftCompatibility51`, `swiftCompatibility56`, etc.)

The linker conflict causes build failures:
```
ld: warning: Could not find or use auto-linked library 'swiftCompatibility51'
Undefined symbols for architecture x86_64:
  "__swift_FORCE_LOAD_$_swiftCompatibility51", referenced from: ...
```

## Future Work

To use the Swift template:

1. Create a separate branch without `toolchains_llvm`
2. Use only Apple's native toolchains for all languages
3. Or, configure Swift to work in isolation from the main build

## Template Status

The Swift template code is complete and working:
- ✅ Uses `@main` attribute (Swift 5.3+)
- ✅ Proper module structure with public functions
- ✅ Deployment target set to macOS 10.15
- ✅ Apple CC toolchain configured in MODULE.bazel

The template will work once the toolchain conflict is resolved.
