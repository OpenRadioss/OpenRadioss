########################################
# ENGINE - GFortran / linuxa64 (AArch64)
########################################

set(cppmach "-DCPP_mach=CPP_p4linux964")
set(cpprel  "-DCPP_rel=75")
set(unused_dummy_arg "")

# MPI (smp / ompi / impi)
include(${CMAKE_CURRENT_LIST_DIR}/common/mpi_linux_ompi_impi.cmake)
set(RELNAME ${arch}${mpi_suf})

# Third-party libraries
include(${CMAKE_CURRENT_LIST_DIR}/common/libs_linuxa64.cmake)

# Optional: MUMPS
include(${CMAKE_CURRENT_LIST_DIR}/common/mumps_gfortran.cmake)

# Build opt_flag (no coupling for linuxa64_gf)
set(OPENMP "-fopenmp")
include(${CMAKE_CURRENT_LIST_DIR}/common/wo_linalg.cmake)
set(opt_flag "${wo_linalg} -DCOMP_GFORTRAN=1 -ffp-contract=off -frounding-math ${OPENMP}")
set(opt_flag "${opt_flag} ${mumps_flag} ${mumps_inc}")

# Common setup
include(${CMAKE_CURRENT_LIST_DIR}/common/precision.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/common/flags_reset_minimal.cmake)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/modules)
set(CMAKE_Fortran_MODDIR_FLAG "-J")
include(${CMAKE_CURRENT_LIST_DIR}/common/module_dir.cmake)

# GFortran version-specific flags
if (CMAKE_C_COMPILER_VERSION VERSION_GREATER 10)
  set(portability "-fallow-argument-mismatch -fallow-invalid-boz -std=legacy")
endif()
set(strict    "-Werror=aliasing -Werror=unused-dummy-argument -Werror=do-subscript -Werror=array-bounds -Werror=tabs -Werror=surprising")
set(strict_wo_mpi "-Werror=aliasing ${unused_dummy_arg} -Werror=do-subscript -Werror=array-bounds -Werror=tabs -Werror=surprising")
set(ARCH_FLAGS "-march=armv8-a -DCOMP_GFORTRAN=1 -fdec-math -DARCH_CPU=ARM")

# Compiler flags (debug/asan/release — no analysis mode for linuxa64_gf)
include(${CMAKE_CURRENT_LIST_DIR}/compiler/gfortran_linux.cmake)

# Legacy fixed-form files
include(CMake_Compilers_v2/legacy_fortran.cmake)

# Linker and libraries
if (static_link STREQUAL "1")
  set(LINK "rt ${mumps_libs} ${zlib_lib} ${md5_lib} -ldl -static-libgfortran -static-libstdc++ -static-libgcc ${mpi_lib} -Wunused-function")
else()
  set(LINK "rt ${mumps_libs} ${zlib_lib} ${md5_lib} ${mpi_lib} -ldl -Wunused-function")
endif()
string(STRIP "${LINK}" LINK)

# Per-file overrides (release only)
set(F_O0_compiler_flags "${ARCH_FLAGS} -nostdinc -O0 ${opt_flag} -w ${portability} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -ffixed-line-length-none ${mpi_flag} ${ADF}")
set(F_O1_compiler_flags "${ARCH_FLAGS} -nostdinc -O1 ${opt_flag} -w ${portability} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -ffixed-line-length-none ${mpi_flag} ${ADF}")
set(F_O2_compiler_flags "${ARCH_FLAGS} -nostdinc -O2 ${opt_flag} -w ${portability} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -ffixed-line-length-none ${mpi_flag} ${ADF}")
set(C_O1_compiler_flags "${ARCH_FLAGS} -O1 ${opt_flag} -w ${precision_flag} ${cppmach} ${cpprel} ${mpi_flag} ${ADF}")

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
  set_source_files_properties(${source_directory}/source/interfaces/int11/i11pen3.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
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
