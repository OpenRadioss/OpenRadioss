##################################################
# STARTER - GFortran Compiler / linuxa64 (AArch64)
##################################################

set(RELNAME ${arch})
set(cppmach "-DCPP_mach=CPP_p4linux964")
set(cpprel  "-DCPP_rel=75")

# Third-party libraries
include(${CMAKE_CURRENT_LIST_DIR}/common/reader_linuxa64.cmake)
set(lapack_lib "${source_directory}/../extlib/lapack-3.10.0/lib_linuxa64/liblapack_gcc.a ${source_directory}/../extlib/lapack-3.10.0/lib_linuxa64/librefblas_gcc.a ${source_directory}/../extlib/lapack-3.10.0/lib_linuxa64/libtmglib_gcc.a")
set(metis_lib  "${source_directory}/../extlib/metis/linuxa64/libmetis_linuxa64.a")
include(${CMAKE_CURRENT_LIST_DIR}/common/libs_linuxa64.cmake)

# Common setup
include(${CMAKE_CURRENT_LIST_DIR}/common/precision.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/common/flags_reset_minimal.cmake)
set(CMAKE_Fortran_MODDIR_FLAG "-J")
include(${CMAKE_CURRENT_LIST_DIR}/common/module_dir.cmake)

# GFortran version-specific portability flags (GCC > 10)
if (CMAKE_C_COMPILER_VERSION VERSION_GREATER 10)
  set(portability "-fallow-argument-mismatch -fallow-invalid-boz -std=legacy")
endif()

set(strict "-Werror=aliasing -Werror=unused-dummy-argument -Werror=do-subscript -Werror=array-bounds -Werror=surprising -Werror=tabs")
set(OPENMP     "-fopenmp")
set(ARCH_FLAGS "-march=armv8-a -DARCH_CPU=ARM")  # ARM-specific flags

# Compilation flags (debug/asan/release)
include(${CMAKE_CURRENT_LIST_DIR}/compiler/gfortran_linux.cmake)

# Legacy fixed-form Fortran files need relaxed flags
include(CMake_Compilers_v2/legacy_fortran.cmake)

# Link libraries
if (static_link STREQUAL "1")
  set(LINK "dl ${flexpipe_lib} ${metis_lib} ${reader_lib} ${lapack_lib} ${zlib_lib} ${md5_lib} -ldl -static-libgfortran -static-libstdc++ -static-libgcc -Wunused-function")
else()
  set(LINK "dl ${flexpipe_lib} ${metis_lib} ${reader_lib} ${lapack_lib} ${zlib_lib} ${md5_lib} -ldl -Wunused-function")
endif()
string(STRIP "${LINK}" LINK)

# Per-file flag overrides
set(F_O0_compiler_flags "-O0 ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} -ffixed-line-length-none ${portability}")
if (NOT debug OR debug STREQUAL "0")
  set_source_files_properties(${source_directory}/source/output/message/inimsg.F
    PROPERTIES COMPILE_FLAGS "${F_O0_compiler_flags}")
endif()

include(${CMAKE_CURRENT_LIST_DIR}/common/no_python.cmake)
