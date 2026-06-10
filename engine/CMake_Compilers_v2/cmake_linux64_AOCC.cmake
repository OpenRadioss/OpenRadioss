########################################
# ENGINE - AMD AOCC / linux64
########################################

set(cppmach "-DCPP_mach=CPP_p4linux964")
set(cpprel  "-DCPP_rel=96")

# MPI (smp / ompi only — no impi for AOCC)
include(${CMAKE_CURRENT_LIST_DIR}/common/mpi_linux_ompi.cmake)
set(RELNAME ${arch}${mpi_suf})

# Third-party libraries
include(${CMAKE_CURRENT_LIST_DIR}/common/libs_linux64.cmake)

# Common setup
include(${CMAKE_CURRENT_LIST_DIR}/common/precision.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/common/flags_reset_minimal.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/common/wo_linalg.cmake)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/modules)
set(CMAKE_Fortran_MODDIR_FLAG "-J")
include(${CMAKE_CURRENT_LIST_DIR}/common/module_dir.cmake)

set(opt_flag "${wo_linalg} -DCOMP_AOCC -ffp-contract=off -frounding-math -fopenmp -fno-unsafe-math-optimizations -fno-fast-math -Wno-null-conversion")

if (sanitize STREQUAL "1")
  set(FSANITIZE "-fsanitize=address -DSANITIZE")
endif()

# Compiler flags (debug/release — no analysis/asan modes for AOCC)
include(${CMAKE_CURRENT_LIST_DIR}/compiler/aocc_linux.cmake)

# Linker and libraries
if (static_link STREQUAL "1")
  set(LINK "rt ${mpi_lib} -ldl -static-libgfortran -static-libstdc++ -static-libgcc -Wunused-function")
else()
  set(LINK "rt ${mpi_lib} -ldl -Wunused-function")
endif()
string(STRIP "${LINK}" LINK)

# Per-file overrides (release only)
set(F_O0_compiler_flags "-O0 ${opt_flag} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -Mextend ${mpi_flag} ${ADF}")
set(F_O1_compiler_flags "-O1 ${opt_flag} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -Mextend ${mpi_flag} ${ADF}")
set(F_O2_compiler_flags "-O2 ${opt_flag} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -Mextend ${mpi_flag} ${ADF}")
set(C_O1_compiler_flags "-O1 ${opt_flag} ${precision_flag} ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} ${mpi_flag} ${ADF}")

if (NOT debug OR debug STREQUAL "0")
  set_source_files_properties(${source_directory}/source/engine/resol_init.F        PROPERTIES COMPILE_FLAGS ${F_O1_compiler_flags})
  set_source_files_properties(${source_directory}/source/engine/resol.F             PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/output/restart/arralloc.F  PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/materials/mat/mat001/m1lawp.F PROPERTIES COMPILE_FLAGS ${F_O1_compiler_flags})
  set_source_files_properties(${source_directory}/source/elements/shell/coqueba/cbasumg3.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/input/redkey0.F            PROPERTIES COMPILE_FLAGS ${F_O1_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/inter3d/i3fri3.F PROPERTIES COMPILE_FLAGS ${F_O1_compiler_flags})
  set_source_files_properties(${source_directory}/source/implicit/dsolve/dsgri7.F   PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/int11/i11cor3.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/intsort/i11pen3.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/int07/i7cor3t.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/int20/i20cor3.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/intsort/i20sto.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/int23/i23cort3.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/int24/i24cort3.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/int25/i25cort3.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/elements/sph/spbuc3.F      PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/elements/sph/spclasv.F     PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/coupling/rad2rad/rad2rad_c.c PROPERTIES COMPILE_FLAGS ${C_O1_compiler_flags})
endif()

include(${CMAKE_CURRENT_LIST_DIR}/common/no_python.cmake)
