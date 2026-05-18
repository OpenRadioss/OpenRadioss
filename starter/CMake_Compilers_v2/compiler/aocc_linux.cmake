## AOCC (AMD) Linux compilation flags.
##
## Required variables (set by calling platform file):
##   cppmach, cpprel, precision_flag
##   zlib_inc, md5_inc
##   source_files, c_source_files, cpp_source_files
##
## Optional variables:
##   ADF  (additional flags from -addflag)
##
## Sets: CMAKE_EXE_LINKER_FLAGS, FSANITIZE (for use in LINK by caller)

if (sanitize STREQUAL "1")
  set(FSANITIZE "-fsanitize=address -DSANITIZE")
endif()

if (debug STREQUAL "1")
  set(fortran_flags "${FSANITIZE} ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} -DCPP_comp=f90 -DCOMP_AOCC=1 -g -O0 -ffp-contract=off -frounding-math -fopenmp -Mextend -Mbackslash ${ADF}")
  set(c_flags       "${FSANITIZE} ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -O0 -g -fopenmp")
  set(cpp_flags     "${FSANITIZE} ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -O0 -g -fopenmp -std=c++11")
else()
  set(fortran_flags "${precision_flag} -DMETIS5 ${cppmach} ${cpprel} -DCPP_comp=f90 -DCOMP_AOCC=1 -O3 -ffp-contract=off -frounding-math -fopenmp -Mextend -Mbackslash ${ADF}")
  set(c_flags       "${precision_flag} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -O3 -fopenmp")
  set(cpp_flags     "${precision_flag} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -O3 -fopenmp -std=c++11")
endif()

set_source_files_properties(${source_files}     PROPERTIES COMPILE_FLAGS "${fortran_flags}")
set_source_files_properties(${c_source_files}   PROPERTIES COMPILE_FLAGS "${c_flags}")
set_source_files_properties(${cpp_source_files} PROPERTIES COMPILE_FLAGS "${cpp_flags}")

set(CMAKE_EXE_LINKER_FLAGS "-rdynamic -no-pie -O2 -fopenmp ${FSANITIZE}")
