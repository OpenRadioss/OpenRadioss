########################################
# ENGINE - ARM Flang / linuxa64
########################################

set(cppmach "-DCPP_mach=CPP_p4linux964")
set(cpprel  "-DCPP_rel=70")

# MPI (smp / ompi only — extra lib for AArch64)
set(OMPI_EXTRA_LIBS "-lmpi_usempif08")
include(${CMAKE_CURRENT_LIST_DIR}/common/mpi_linux_ompi.cmake)
set(RELNAME ${arch}${mpi_suf})

# Third-party libraries
include(${CMAKE_CURRENT_LIST_DIR}/common/libs_linuxa64.cmake)

# Common setup
include(${CMAKE_CURRENT_LIST_DIR}/common/precision.cmake)
# Partial flag reset — matching legacy file
set(CMAKE_Fortran_FLAGS_DEBUG " ")
set(CMAKE_Fortran_FLAGS_RELEASE " ")
set(CMAKE_C_FLAGS_RELEASE " ")
set(CMAKE_CPP_FLAGS_RELEASE " ")
set(CMAKE_CXX_FLAGS_RELEASE " ")
include(${CMAKE_CURRENT_LIST_DIR}/common/wo_linalg.cmake)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/modules_${EXEC_NAME})
set(CMAKE_Fortran_MODDIR_FLAG "-module ")
include(${CMAKE_CURRENT_LIST_DIR}/common/module_dir.cmake)

set(Vect_opt    "-march=armv8-a -D COMP_ARMFLANG=1 -D ARCH_CPU=ARM -fopenmp")
set(Vect_precise "-nofma -ffp-contract=off -fno-unsafe-math-optimizations -fno-fast-math -fveclib=none")
set(Fortran     "-ffixed-line-length-none")

# Compiler flags (debug/asan/release)
include(${CMAKE_CURRENT_LIST_DIR}/compiler/armflang_linuxa64.cmake)

# Linker and libraries
set(LINK "dl ${mpi_lib} ${zlib_lib} ${md5_lib} -lrt -lm -lstdc++")
string(STRIP "${LINK}" LINK)

# Per-file overrides (release only)
set(F_O0 "-O0 ${Vect_opt} ${Vect_precise} ${precision_flag} ${Fortran} ${mpi_flag} ${wo_linalg} ${cppmach} ${cpprel} ${h3d_inc}")
set(F_O1 "-O1 ${Vect_opt} ${Vect_precise} ${precision_flag} ${Fortran} ${mpi_flag} ${wo_linalg} ${cppmach} ${cpprel} ${h3d_inc}")
set(F_O2 "-O3 ${Vect_opt} ${Vect_precise} ${precision_flag} ${Fortran} ${mpi_flag} ${wo_linalg} ${cppmach} ${cpprel} ${h3d_inc}")

if (NOT debug OR debug STREQUAL "0")
  set_source_files_properties(${source_directory}/source/engine/resol_init.F        PROPERTIES COMPILE_FLAGS ${F_O1})
  set_source_files_properties(${source_directory}/source/engine/resol.F             PROPERTIES COMPILE_FLAGS ${F_O2})
  set_source_files_properties(${source_directory}/source/elements/shell/coqueba/cbasumg3.F PROPERTIES COMPILE_FLAGS ${F_O2})
  set_source_files_properties(${source_directory}/source/input/redkey0.F            PROPERTIES COMPILE_FLAGS ${F_O1})
  set_source_files_properties(${source_directory}/source/interfaces/inter3d/i3fri3.F PROPERTIES COMPILE_FLAGS ${F_O1})
  set_source_files_properties(${source_directory}/source/output/restart/arralloc.F  PROPERTIES COMPILE_FLAGS ${F_O2})

  if (CMAKE_C_COMPILER_VERSION VERSION_GREATER 18)
    set_source_files_properties(${source_directory}/source/elements/sh3n/coque3n/c3bilan.F PROPERTIES COMPILE_FLAGS ${F_O0})
    set_source_files_properties(${source_directory}/source/assembly/damping_vref_rby.F90  PROPERTIES COMPILE_FLAGS ${F_O2})
    set_source_files_properties(${source_directory}/source/assembly/damping_vref_sum6_rby.F90 PROPERTIES COMPILE_FLAGS ${F_O0})
  endif()
endif()

include(${CMAKE_CURRENT_LIST_DIR}/common/no_python.cmake)
