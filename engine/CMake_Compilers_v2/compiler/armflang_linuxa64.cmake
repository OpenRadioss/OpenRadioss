## ARM Flang (AArch64 Linux) compilation flags.
## Required: cppmach, cpprel, precision_flag, h3d_inc, zlib_inc, md5_inc, mpi_flag, ADF
##           Vect_opt     e.g. "-march=armv8-a -D COMP_ARMFLANG=1 -D ARCH_CPU=ARM -fopenmp"
##           Vect_precise e.g. "-nofma -ffp-contract=off -fno-unsafe-math-optimizations -fno-fast-math -fveclib=none"
##           Fortran      e.g. "-ffixed-line-length-none"
##           wo_linalg

if (debug STREQUAL "1" OR debug STREQUAL "asan")
  set(fortran_flags "-g -O0 ${Vect_opt} ${Vect_precise} ${wo_linalg} ${precision_flag} ${Fortran} ${mpi_flag} ${cppmach} ${cpprel} ${h3d_inc}")
  set(c_flags       "-g -O0 ${Vect_opt} ${wo_linalg} ${precision_flag} ${mpi_flag} ${cppmach} ${cpprel} ${zlib_inc} ${h3d_inc} ${md5_inc}")
  set(cpp_flags     "-g -O0 ${Vect_opt} ${wo_linalg} ${precision_flag} ${mpi_flag} ${cppmach} ${cpprel} ${zlib_inc} ${h3d_inc} ${md5_inc} -std=c++11")
else()
  set(fortran_flags "${wo_linalg} ${Vect_opt} ${precision_flag} ${Fortran} ${mpi_flag} ${cppmach} ${cpprel} -O3 ${Vect_precise} ${h3d_inc}")
  set(c_flags       "${wo_linalg} ${Vect_opt} ${precision_flag} ${mpi_flag} ${cppmach} ${cpprel} ${zlib_inc} ${h3d_inc} ${md5_inc} -O2")
  set(cpp_flags     "${wo_linalg} ${Vect_opt} ${precision_flag} ${mpi_flag} ${cppmach} ${cpprel} ${zlib_inc} ${h3d_inc} ${md5_inc} -O2 -std=c++11")
endif()

set_source_files_properties(${source_files}     PROPERTIES COMPILE_FLAGS "${fortran_flags}")
set_source_files_properties(${c_source_files}   PROPERTIES COMPILE_FLAGS "${c_flags}")
set_source_files_properties(${cpp_source_files} PROPERTIES COMPILE_FLAGS "${cpp_flags}")

set(CMAKE_EXE_LINKER_FLAGS "-march=armv8-a -ffp-contract=off -fno-unsafe-math-optimizations -fno-fast-math -fopenmp -rdynamic")
