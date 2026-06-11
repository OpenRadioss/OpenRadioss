## NVIDIA HPC SDK (nvfortran/nvc/nvc++) compilation flags.
## Required: cppmach, cpprel, precision_flag, h3d_inc, zlib_inc, md5_inc, mpi_flag, ADF
##           fort_flags  base Fortran flags (-mp, -Mnofma, -Mextend, -I<sdk_inc> etc.)
##           wo_linalg   set unconditionally for NVHPC (no MUMPS support)

if (debug STREQUAL "1")
  set(fortran_flags "-Wall -g -O0 -Mbounds ${fort_flags} ${wo_linalg} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -DCOMP_NVFORTRAN=1 ${mpi_flag} ${ADF}")
  set(c_flags       "-g -O0 -mp ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${cppmach} ${cpprel} ${mpi_flag}")
  set(cpp_flags     "-g -O0 -mp ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${cppmach} ${cpprel} -std=c++14 ${mpi_flag}")
else()
  set(fortran_flags "-O2 ${fort_flags} ${wo_linalg} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -DCOMP_NVFORTRAN=1 ${mpi_flag} ${ADF}")
  set(c_flags       "-w -O2 -mp ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${cppmach} ${cpprel} ${mpi_flag}")
  set(cpp_flags     "-w -O2 -mp ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${cppmach} ${cpprel} -std=c++14 ${mpi_flag}")
endif()

set_source_files_properties(${source_files}     PROPERTIES COMPILE_FLAGS "${fortran_flags}")
set_source_files_properties(${c_source_files}   PROPERTIES COMPILE_FLAGS "${c_flags}")
set_source_files_properties(${cpp_source_files} PROPERTIES COMPILE_FLAGS "${cpp_flags}")

set(CMAKE_EXE_LINKER_FLAGS "-Wl,--export-dynamic -Wl,-no-pie -mp")
string(STRIP "${CMAKE_EXE_LINKER_FLAGS}" CMAKE_EXE_LINKER_FLAGS)
