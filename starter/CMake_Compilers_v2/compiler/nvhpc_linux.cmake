## NVIDIA HPC (nvfortran) Linux compilation flags.
##
## Required variables (set by calling platform file):
##   cppmach, cpprel, precision_flag
##   zlib_inc
##   fort_flags   (NVHPC-specific Fortran flags set by platform file)
##   source_files, c_source_files, cpp_source_files
##
## Optional variables:
##   ADF
##
## Sets: CMAKE_EXE_LINKER_FLAGS

if (debug STREQUAL "1")
  set(fortran_flags "-Wall ${fort_flags} ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} -DCPP_comp=f90 -DCOMP_NVFORTRAN=1 -g -O0 -fbacktrace ${ADF}")
  set(c_flags       "${precision_flag} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} -O0 -g")
  set(cpp_flags     "${precision_flag} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} -O0 -g -std=c++14")
else()
  set(fortran_flags "${precision_flag} ${fort_flags} -DMETIS5 ${cppmach} ${cpprel} -DCPP_comp=f90 -DCOMP_NVFORTRAN=1 -O2 ${ADF}")
  set(c_flags       "-w ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} -O2")
  set(cpp_flags     "-w ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} -O2 -std=c++14")
endif()

set_source_files_properties(${source_files}     PROPERTIES COMPILE_FLAGS "${fortran_flags}")
set_source_files_properties(${c_source_files}   PROPERTIES COMPILE_FLAGS "${c_flags}")
set_source_files_properties(${cpp_source_files} PROPERTIES COMPILE_FLAGS "${cpp_flags}")

set(CMAKE_EXE_LINKER_FLAGS "-Wl,--export-dynamic -Wl,-no-pie -O2")
