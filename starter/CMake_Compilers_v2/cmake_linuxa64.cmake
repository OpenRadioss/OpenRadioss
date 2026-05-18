##############################################
# STARTER - ARM Flang Compiler / linuxa64
##############################################
# ARM Flang uses -nofma, -fveclib=none, -armpl, -march=armv8-a.
# Module directory uses -module (not -J as in GFortran).

set(RELNAME ${arch})
set(cppmach "-DCPP_mach=CPP_p4linux964")
set(cpprel  "-DCPP_rel=70")

# Third-party libraries
include(${CMAKE_CURRENT_LIST_DIR}/common/reader_linuxa64.cmake)
set(lapack_lib "${source_directory}/../extlib/lapack-3.10.0/lib_linux64_gf/liblapack.a ${source_directory}/../extlib/lapack-3.10.0/lib_linux64_gf/librefblas.a ${source_directory}/../extlib/lapack-3.10.0/lib_linux64_gf/libtmglib.a")
set(metis_lib  "${source_directory}/../extlib/metis/linuxa64/libmetis_linuxa64.a")
include(${CMAKE_CURRENT_LIST_DIR}/common/libs_linuxa64.cmake)

# Common setup
include(${CMAKE_CURRENT_LIST_DIR}/common/precision.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/common/flags_reset_minimal.cmake)
set(CMAKE_Fortran_MODDIR_FLAG "-module ")
include(${CMAKE_CURRENT_LIST_DIR}/common/module_dir.cmake)

# ARM Flang-specific parameters (passed to compiler/armflang_linuxa64.cmake)
set(ARCH_OPT    "-O2 -march=armv8-a -DCOMP_ARMFLANG=1 -DARCH_CPU=ARM -nofma -fopenmp -fveclib=none")
set(FLAG_PRECISE "-ffp-contract=off -fno-unsafe-math-optimizations -fno-fast-math")
set(Fortran_base "-DCPP_comp=f90 -ffixed-line-length-none -fno-backslash")

# Compilation flags (debug/asan/release)
include(${CMAKE_CURRENT_LIST_DIR}/compiler/armflang_linuxa64.cmake)

set(LINK "dl ${metis_lib} ${xml_lib} ${flexpipe_lib} ${reader_lib} ${flexpipe} ${Tet_mesher_lib} ${zlib_lib} ${md5_lib} -lrt -lm -lstdc++")

# Per-file flag overrides
set(C_O0_compiler_flags "${precision_flag} -DMETIS5 ${cppmach} ${cpprel} -O0")
set_source_files_properties(${source_directory}/com/fvmbag/tiangle.c
  PROPERTIES COMPILE_FLAGS "${C_O0_compiler_flags}")
