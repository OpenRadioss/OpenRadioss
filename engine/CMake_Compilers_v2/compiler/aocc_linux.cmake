## AMD AOCC/Flang Linux compilation flags.
## Required: cppmach, cpprel, precision_flag, h3d_inc, zlib_inc, md5_inc,
##           opt_flag, mpi_flag, FSANITIZE, ADF

if (debug STREQUAL "1")
  set(fortran_flags "${FSANITIZE} -O0 -g -Wunused ${opt_flag} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -Mextend ${mpi_flag} ${ADF}")
  set(c_flags       "${FSANITIZE} ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${cppmach} ${cpprel} -O0 -g -fopenmp")
  set(cpp_flags     "${FSANITIZE} ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${cppmach} ${cpprel} -O0 -g -fopenmp -std=c++11")
else()
  set(fortran_flags "-O3 ${opt_flag} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -Mextend ${mpi_flag} ${ADF}")
  set(c_flags       "-O3 ${h3d_inc} ${zlib_inc} ${md5_inc} ${opt_flag} ${precision_flag} ${cppmach} ${cpprel}")
  set(cpp_flags     "-O3 ${h3d_inc} ${zlib_inc} ${md5_inc} ${opt_flag} ${precision_flag} ${cppmach} ${cpprel} -std=c++11")
endif()

set_source_files_properties(${source_files}     PROPERTIES COMPILE_FLAGS "${fortran_flags}")
set_source_files_properties(${c_source_files}   PROPERTIES COMPILE_FLAGS "${c_flags}")
set_source_files_properties(${cpp_source_files} PROPERTIES COMPILE_FLAGS "${cpp_flags}")

set(CMAKE_EXE_LINKER_FLAGS "-rdynamic -fopenmp -ldl -lrt ${zlib_lib} ${md5_lib} -lstdc++ ${FSANITIZE}")
