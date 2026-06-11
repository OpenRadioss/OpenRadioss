# CMake_Compilers_v2 — Refactored Engine Build System

This directory contains the refactored CMake compilation settings for the OpenRadioss Engine binary.
It mirrors the same refactoring already done for the Starter in `starter/CMake_Compilers_v2/`.

## Motivation

The legacy `engine/CMake_Compilers/` system uses one monolithic `.txt` file per target platform
(~150–400 lines each).  Every change to a shared flag (e.g. an OpenMP flag that affects all
compilers) had to be replicated across all ten files.  `CMake_Compilers_v2/` decomposes each file
into small, single-responsibility snippets that platform files simply `include()`.

## Directory Layout

```
CMake_Compilers_v2/
├── README.md                        ← this file
├── legacy_fortran.cmake             ← copied from CMake_Compilers/ (per-file overrides)
├── platforms.txt                    ← copied from CMake_Compilers/ (valid arch list)
│
├── common/                          ← reusable snippets included by platform files
│   ├── precision.cmake              ← single/double precision selector
│   ├── flags_reset_minimal.cmake    ← clear only DEBUG + RELEASE flag sets (GFortran/AOCC)
│   ├── flags_reset_full.cmake       ← clear all flag sets (Intel/NVHPC/Win)
│   ├── wo_linalg.cmake              ← set WITHOUT_LINALG when MUMPS not enabled
│   ├── module_dir.cmake             ← print module-directory status message
│   ├── no_python.cmake              ← disable Python linkage when no_python=1
│   ├── libs_linux64.cmake           ← h3d/zlib/md5 library paths for x86-64 Linux
│   ├── libs_linuxa64.cmake          ← h3d/zlib/md5 library paths for AArch64 Linux
│   ├── libs_win64.cmake             ← h3d/zlib/md5 library paths for Windows
│   ├── mpi_linux_ompi_impi.cmake    ← MPI (smp/ompi/impi) for Linux, supports OMPI_EXTRA_LIBS
│   ├── mpi_linux_ompi.cmake         ← MPI (smp/ompi only) for AOCC and ARM Flang
│   ├── mpi_linux_ompi_nvhpc.cmake   ← MPI for NVHPC (HPC-X discovery)
│   ├── mpi_win64.cmake              ← MPI (smp/impi) + MKL for Windows
│   ├── mumps_gfortran.cmake         ← MUMPS solver for GFortran (scalapack/lapack deps)
│   ├── mumps_intel_linux.cmake      ← MUMPS solver for Intel Linux (MKL-based)
│   ├── mumps_win64.cmake            ← MUMPS solver for Intel Windows (MKL .lib files)
│   └── coupling_gfortran.cmake      ← preCICE/CWIPI coupling (linux64_gf only)
│
├── compiler/                        ← per-compiler-family flag files
│   ├── gfortran_linux.cmake         ← GFortran debug/analysis/asan/release flags
│   ├── intel_linux.cmake            ← Intel Linux debug/chkb/asan/release (ifort + ifx)
│   ├── intel_win64.cmake            ← Intel Windows debug/chkb/release flags
│   ├── aocc_linux.cmake             ← AOCC debug/release flags
│   ├── nvhpc_linux.cmake            ← NVHPC debug/release flags
│   └── armflang_linuxa64.cmake      ← ARM Flang debug/asan/release flags
│
├── cmake_linux64_gf.cmake           ← GFortran x86-64
├── cmake_linux64_ifort.cmake        ← Intel ifort x86-64
├── cmake_linux64_ifx.cmake          ← Intel ifx x86-64
├── cmake_linux64_AOCC.cmake         ← AOCC x86-64
├── cmake_linux64_nvidia.cmake       ← NVHPC x86-64
├── cmake_linuxa64.cmake             ← ARM Flang AArch64
├── cmake_linuxa64_gf.cmake          ← GFortran AArch64
├── cmake_win64.cmake                ← Intel ifort Win64 (AVX2)
├── cmake_win64_ifort.cmake          ← Intel ifort Win64 (SSE3)
└── cmake_win64_sse3.cmake           ← Intel ifort Win64 SSE3 variant
```

## Supported Platforms

| Platform file              | Compiler     | OS      | ISA       | MPI             | MUMPS | Coupling |
|----------------------------|--------------|---------|-----------|-----------------|-------|----------|
| `cmake_linux64_gf`         | GFortran     | Linux   | x86-64    | smp/ompi/impi   | ✓     | ✓        |
| `cmake_linux64_ifort`      | Intel ifort  | Linux   | x86-64    | smp/ompi/impi   | ✓     |          |
| `cmake_linux64_ifx`        | Intel ifx    | Linux   | x86-64    | smp/ompi/impi   | ✓     |          |
| `cmake_linux64_AOCC`       | AOCC/Flang   | Linux   | x86-64    | smp/ompi        |       |          |
| `cmake_linux64_nvidia`     | NVHPC        | Linux   | x86-64    | smp/ompi (HPC-X)|       |          |
| `cmake_linuxa64`           | ARM Flang    | Linux   | AArch64   | smp/ompi        |       |          |
| `cmake_linuxa64_gf`        | GFortran     | Linux   | AArch64   | smp/ompi/impi   | ✓     |          |
| `cmake_win64`              | Intel ifort  | Windows | x86-64 AVX2| smp/impi       | ✓     |          |
| `cmake_win64_ifort`        | Intel ifort  | Windows | x86-64 SSE3| smp/impi       | ✓     |          |
| `cmake_win64_sse3`         | Intel ifort  | Windows | x86-64 SSE3| smp/impi       | ✓     |          |

## Uniform Structure of Each Platform File

Every platform `.cmake` file follows this pattern:

```cmake
# 1. Compiler executable paths
# 2. Precision (include common/precision.cmake)
# 3. Flag reset (include common/flags_reset_*.cmake)
# 4. WITHOUT_LINALG default (include common/wo_linalg.cmake)
# 5. Module directory message (include common/module_dir.cmake)
# 6. No-Python flag (include common/no_python.cmake)
# 7. Architecture SIMD flags (ARCH_FLAGS)
# 8. Platform-specific defines / CPP flags
# 9. Compiler flags (include compiler/<family>.cmake)
# 10. Library paths (include common/libs_<os>.cmake)
# 11. MPI settings (include common/mpi_<variant>.cmake)
# 12. MUMPS settings (include common/mumps_<variant>.cmake, where supported)
# 13. Coupling (include common/coupling_gfortran.cmake, linux64_gf only)
# 14. Per-file overrides (include CMake_Compilers_v2/legacy_fortran.cmake)
```

## How to Use

### Activate v2 (recommended)

Pass `-cmake_ver=v2` to `build_script.sh`:

```bash
cd engine
./build_script.sh -arch=linux64_gf -mpi=ompi -cmake_ver=v2
```

The build directory will be named `cbuildv2_<exec_name>` to avoid collisions with legacy builds.

### Legacy (default)

Omit `-cmake_ver` to use the original `CMake_Compilers/` system unchanged:

```bash
./build_script.sh -arch=linux64_gf -mpi=ompi
```

### CMake Directly

```bash
cmake .. -Darch=linux64_gf -DMPI=ompi -Dprecision=dp -Ddebug=0 \
         -Dcmake_ver=v2 \
         -DEXEC_NAME=engine_linux64_gf_ompi \
         -DCMAKE_Fortran_COMPILER=gfortran \
         -DCMAKE_C_COMPILER=gcc \
         -DCMAKE_BUILD_TYPE=Release
```

## Engine-Specific Differences from Starter v2

- **MPI support**: Engine supports `smp`, `ompi`, `impi` via `common/mpi_*.cmake` snippets.
  Starter has no MPI.
- **MUMPS solver**: `common/mumps_*.cmake` snippets handle GFortran, Intel Linux, and Windows.
- **H3D / zlib / md5**: Required for all platforms via `common/libs_<os>.cmake`.
- **Coupling**: `common/coupling_gfortran.cmake` enables preCICE or CWIPI (linux64_gf only).
- **NVHPC**: Always sets `WITHOUT_LINALG` (no MUMPS support for NVHPC).
- **ARM Flang** (`linuxa64`): Sets `OMPI_EXTRA_LIBS=-lmpi_usempif08` for OpenMPI.
- **ifx + ompi**: Sets `cpprel="-DCPP_rel=40"` to work around a known compiler issue.

## Adding a New Platform

1. Create `cmake_<new_arch>.cmake` following the uniform structure above.
2. Include the appropriate `common/` and `compiler/` snippets.
3. Add the arch name to `platforms.txt`.
4. Test with `./build_script.sh -arch=<new_arch> -cmake_ver=v2`.
