##################################################
# STARTER - NVIDIA HPC (nvfortran) Compiler / linux64
##################################################

set(RELNAME ${arch})
set(cppmach "-DCPP_mach=CPP_p4linux964")
set(cpprel  "-DCPP_rel=80")

# Detect static GFortran/GCC libraries needed when linking with nvfortran
execute_process(COMMAND gfortran -print-file-name=libgfortran.a OUTPUT_VARIABLE LIBGFORTRAN_PATH OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND gfortran -print-file-name=libgcc.a      OUTPUT_VARIABLE LIBGCC_PATH      OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND gfortran -print-file-name=libquadmath.a OUTPUT_VARIABLE LIBQUADMATH_PATH OUTPUT_STRIP_TRAILING_WHITESPACE)

# Third-party libraries
include(${CMAKE_CURRENT_LIST_DIR}/common/reader_linux64.cmake)
set(lapack_lib "${source_directory}/../extlib/lapack-3.10.0/lib_linux64_gf/liblapack.a ${source_directory}/../extlib/lapack-3.10.0/lib_linux64_gf/librefblas.a ${source_directory}/../extlib/lapack-3.10.0/lib_linux64_gf/libtmglib.a")
set(metis_lib  "${source_directory}/../extlib/metis/linux64/libmetis_linux64_gcc.a")
include(${CMAKE_CURRENT_LIST_DIR}/common/libs_linux64.cmake)

# Common setup
include(${CMAKE_CURRENT_LIST_DIR}/common/precision.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/common/flags_reset_minimal.cmake)
# nvfortran uses -module (not -J as in GFortran)
set(CMAKE_Fortran_MODDIR_FLAG "-module ")
include(${CMAKE_CURRENT_LIST_DIR}/common/module_dir.cmake)

# NVHPC-specific Fortran flags (passed to compiler/nvhpc_linux.cmake via fort_flags)
set(fort_flags "-Mnofma -mp -traceback -Mextend -Mnostdinc -Mbounds -Munroll -Mvect=simd -Minform=warn -Mdclchk -Minfo=all -module /opt/nvidia/hpc_sdk/Linux_x86_64/24.7/compilers/include")

# Compilation flags (debug/release)
include(${CMAKE_CURRENT_LIST_DIR}/compiler/nvhpc_linux.cmake)

# Link libraries
if (static_link STREQUAL "1")
  set(LINK "dl ${flexpipe_lib} ${metis_lib} ${reader_lib} ${lapack_lib} ${zlib_lib} ${md5_lib} ${LIBGFORTRAN_PATH} ${LIBGCC_PATH} ${LIBQUADMATH_PATH} -ldl -Bstatic_pgi -Bstatic_c++libs -Bstatic_gcc")
else()
  set(LINK "dl ${flexpipe_lib} ${metis_lib} ${reader_lib} ${lapack_lib} ${zlib_lib} ${md5_lib} ${LIBGFORTRAN_PATH} ${LIBGCC_PATH} ${LIBQUADMATH_PATH} -ldl")
endif()
string(STRIP "${LINK}" LINK)

# Per-file flag overrides
set(F_O0_compiler_flags "-O0 ${fort_flags} ${precision_flag} -DMETIS5 ${cppmach} ${cpprel}")
if (NOT debug OR debug STREQUAL "0")
  set_source_files_properties(${source_directory}/source/output/message/inimsg.F
    PROPERTIES COMPILE_FLAGS "${F_O0_compiler_flags}")
endif()

include(${CMAKE_CURRENT_LIST_DIR}/common/no_python.cmake)
