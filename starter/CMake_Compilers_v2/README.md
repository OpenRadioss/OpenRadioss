# CMake_Compilers_v2 — Refactored Compilation Architecture

This directory is the second-generation CMake configuration for the OpenRadioss
**starter** binary.  It replaces `../CMake_Compilers/` (the v1 `.txt`-based system)
and can be selected at build time with the `-cmake_ver=v2` flag passed to
`build_script.sh`.

## Motivation

The legacy system contained ~1 500 lines spread across 10 platform files with
extensive duplication:

| Metric | v1 (`CMake_Compilers/`) | v2 (`CMake_Compilers_v2/`) |
|--------|------------------------|---------------------------|
| Total lines | ~1 496 | ~862 (−42 %) |
| `md5_inc` definitions | 10 (one per platform) | 3 (one per OS family) |
| `precision STREQUAL "sp"` blocks | 30 | 1 |
| Debug/release branching in platform files | everywhere | 0 |

## Directory Layout

```
CMake_Compilers_v2/
│
│   README.md                  ← this file
│   platforms.txt              ← list of supported -arch= values (copied from v1)
│   legacy_fortran.cmake       ← per-file flags for fixed-form .F files (copied from v1)
│
├── cmake_<arch>.cmake         ← 10 platform entry-point files (one per -arch= value)
│
├── common/                    ← reusable snippets included by every platform file
│   ├── precision.cmake
│   ├── flags_reset_minimal.cmake
│   ├── flags_reset_full.cmake
│   ├── module_dir.cmake
│   ├── no_python.cmake
│   ├── mkl.cmake
│   ├── reader_linux64.cmake
│   ├── reader_linuxa64.cmake
│   ├── reader_win64.cmake
│   ├── libs_linux64.cmake
│   ├── libs_linuxa64.cmake
│   └── libs_win64.cmake
│
└── compiler/                  ← compiler-family flag sets (debug / asan / release)
    ├── gfortran_linux.cmake
    ├── intel_linux.cmake
    ├── intel_win64.cmake
    ├── aocc_linux.cmake
    ├── nvhpc_linux.cmake
    └── armflang_linuxa64.cmake
```

## Platform Files

Each `cmake_<arch>.cmake` is the entry point selected by `CMakeLists.txt` based
on the `-arch=` argument.  The supported values are listed in `platforms.txt`.

| File | Compiler | OS |
|------|----------|----|
| `cmake_linux64_gf.cmake`    | GFortran          | Linux x86-64 |
| `cmake_linux64_ifx.cmake`   | Intel oneAPI ifx  | Linux x86-64 |
| `cmake_linux64_ifort.cmake` | Intel oneAPI ifort| Linux x86-64 |
| `cmake_linux64_AOCC.cmake`  | AMD AOCC / Flang  | Linux x86-64 |
| `cmake_linux64_nv.cmake`    | NVHPC / nvfortran | Linux x86-64 |
| `cmake_linuxa64.cmake`      | ARM Flang         | Linux AArch64 |
| `cmake_linuxa64_gf.cmake`   | GFortran          | Linux AArch64 |
| `cmake_win64.cmake`         | Intel oneAPI      | Windows x64 |
| `cmake_win64_ifort.cmake`   | Intel ifort       | Windows x64 |
| `cmake_win64_sse3.cmake`    | Intel oneAPI (SSE3)| Windows x64 |

## Uniform Platform File Structure

Every platform file follows this exact structure in order — no exceptions:

```cmake
# 1. Identity
set(RELNAME ...)       # usually ${arch}
set(cppmach ...)       # CPP macro for machine type
set(cpprel  ...)       # CPP macro for release

# 2. Reader library (HM reader vs open_reader)
include(.../common/reader_<os>.cmake)

# 3. Optional: lapack_lib, metis_lib, MKL_Inc/MKL_Lib
set(lapack_lib ...)
set(metis_lib  ...)

# 4. Third-party include/link paths (zlib, md5)
include(.../common/libs_<os>.cmake)

# 5. Common build setup
include(.../common/precision.cmake)
include(.../common/flags_reset_{minimal|full}.cmake)
# win64 only: pre-set CMAKE_Fortran_MODULE_DIRECTORY here
set(CMAKE_Fortran_MODDIR_FLAG ...)
include(.../common/module_dir.cmake)

# 6. Compiler-family parameters (set before including the compiler file)
set(ARCH_FLAGS ...)    # e.g. "" for x86, "-march=armv8-a ..." for ARM
set(OPENMP     ...)    # e.g. "-fopenmp"
set(FPP_FLAG   ...)    # Windows only: "/fpp" or "/nofpp" (Ninja workaround)
set(VECT_OPT   ...)    # Windows only: vectorisation flags

# 7. Compiler flags (sets CMAKE_Fortran/C/CXX_FLAGS per source file)
include(.../compiler/<family>.cmake)

# 8. Optional: legacy fixed-form files
include(CMake_Compilers_v2/legacy_fortran.cmake)

# 9. Linker flags and libraries
set(CMAKE_EXE_LINKER_FLAGS ...)
set(LINK ...)

# 10. Per-file compiler flag overrides (release-only blocks guarded by:
#     if (NOT debug OR debug STREQUAL "0") ... endif())

# 11. Python suppression
include(.../common/no_python.cmake)
```

## Common Snippets (`common/`)

### `precision.cmake`
Reads the `precision` variable (passed via `-precision=sp` to `build_script.sh`)
and sets `precision_flag` to either `-DMYREAL4` (single) or `-DMYREAL8` (double).

### `flags_reset_minimal.cmake` / `flags_reset_full.cmake`
Clear CMake's built-in `CMAKE_Fortran_FLAGS_DEBUG`, `CMAKE_Fortran_FLAGS_RELEASE`
(and the base `CMAKE_Fortran_FLAGS` for the *full* variant) so that all flags are
explicit and reproducible.

### `module_dir.cmake`
Sets `CMAKE_Fortran_MODULE_DIRECTORY`.  The default is
`${CMAKE_BINARY_DIR}/CMakeFiles/modules`.  Windows platform files pre-set this
variable to `${CMAKE_BINARY_DIR}/CMakeFiles/{EXEC_NAME}/modules` before including
this snippet.

### `reader_<os>.cmake`
Selects the reader library path based on the `USE_OPEN_READER` variable:
- `1` → `libopen_reader_*` from `../exec/`
- otherwise → `libhm_reader_*` from `../extlib/hm_reader/`

### `libs_<os>.cmake`
Sets `zlib_inc`, `zlib_lib`, `md5_inc`, `md5_lib` for the target OS
(one file per OS family: linux64, linuxa64, win64).

### `no_python.cmake`
Appends `-DNO_PYTHON` to every source file when Python is not available or not
requested.

### `mkl.cmake`
Discovers the Intel MKL installation and sets `MKL_Inc` / `MKL_Lib` when MKL is
available (Linux only).

## Compiler Family Files (`compiler/`)

Each file sets per-source-file compile flags for every supported build mode.
Platform files must set the required input variables before including these files.

### `gfortran_linux.cmake`
Shared by `cmake_linux64_gf` and `cmake_linuxa64_gf`.

Required inputs: `cppmach`, `cpprel`, `precision_flag`, `zlib_inc`, `md5_inc`,
`ARCH_FLAGS`, `OPENMP`.  
Optional inputs: `portability`, `strict`, `error_uninitialized`, `ADF`.

Build modes: `debug=1` · `debug=analysis` · `debug=asan` · release (default).

### `intel_linux.cmake`
Shared by `cmake_linux64_ifx` and `cmake_linux64_ifort`.

Build modes: `debug=1` · `debug=chkb` · `debug=asan` · release.

### `intel_win64.cmake`
Shared by all three Windows platform files.

Required inputs: `FPP_FLAG` (set to `/nofpp` when the Ninja generator is used,
`/fpp` otherwise), `VECT_OPT` (vectorisation flags including optional `/Qfma-`).  
Sets `debug_link` (used by the platform file to construct `CMAKE_EXE_LINKER_FLAGS`).

Build modes: `debug=1` · `debug=chkb` · release.

### `aocc_linux.cmake`
AMD AOCC / Flang (x86-64).  Uses `-Mextend` for fixed-form line length instead of
`-ffixed-line-length-none`.

Build modes: `debug=1` · release.

### `nvhpc_linux.cmake`
NVIDIA HPC SDK / nvfortran.

Required input: `fort_flags` (additional Fortran flags set by the platform file).  
Does **not** add `md5_inc` to C/CXX flags (nvfortran C compiler does not need it).

Build modes: `debug=1` · release.

### `armflang_linuxa64.cmake`
ARM Flang (AArch64 Linux).

Required inputs: `ARCH_OPT` (e.g. `-march=armv8.2-a`), `FLAG_PRECISE`,
`Fortran_base` (base Fortran flags).  
`debug=asan` falls back to `debug=1` behaviour (no ASan support on AArch64).

Build modes: `debug=1` (also handles `debug=asan`) · release.

## How to Select v2 at Build Time

Pass `-cmake_ver=v2` to `build_script.sh`:

```bash
./build_script.sh -arch=linux64_gf -cmake_ver=v2
```

The build directory will be named `cbuildv2_<arch>_<nproc>` instead of the
default `cbuild_<arch>_<nproc>`, allowing both versions to coexist side by side.

The selection logic lives in `starter/CMakeLists.txt`: when `cmake_ver` is set,
`cmake_compilers_root` points to `CMake_Compilers_v2/` and the file extension
`cmake_compilers_ext` is set to `.cmake` (instead of `.txt`).

## Adding a New Platform

1. Create `cmake_<new_arch>.cmake` following the uniform structure above.
2. Choose the nearest compiler family file from `compiler/`, or add a new one.
3. Add `<new_arch>` to `platforms.txt`.
4. If the platform uses a new OS family, add the corresponding
   `common/libs_<os>.cmake` and `common/reader_<os>.cmake` snippets.
