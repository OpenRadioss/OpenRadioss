## Intel compiler Linux flags — shared by linux64_ifort and linux64_ifx.
## Required variables (set by caller before include):
##   cppmach, cpprel, precision_flag, zlib_inc, md5_inc, h3d_inc
##   Fortran            base Fortran flags (wo_linalg, -ftz, -extend-source etc.)
##   opt_flag_release   release optimization flags (ifort vs ifx differ here)
##   optc_flag_release  release C optimization flags
##   linalg_flags       MUMPS include/flags for Fortran (empty if no MUMPS)
##   linalg_cflags      MUMPS flags for C/C++ (empty if no MUMPS)
##   mpi_flag, mpi_lib
##   mumps_libs, MKL_lib
##   ADF

set(opt_flag_debug  "-g3 -traceback -no-fma -O0 -fp-model precise -qopenmp")
set(optc_flag_debug "-g3 -no-fma -O0 -fp-model precise -qopenmp")

if (debug STREQUAL "1")
  set(fortran_flags "${opt_flag_debug} ${linalg_flags} ${precision_flag} ${Fortran} ${mpi_flag} ${cppmach} ${cpprel} ${ADF}")
  set(c_flags       "${optc_flag_debug} ${linalg_cflags} ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${mpi_flag} ${cppmach} ${cpprel} -O2")
  set(cpp_flags     "${optc_flag_debug} ${linalg_cflags} ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${mpi_flag} ${cppmach} ${cpprel} -O2 -std=c++11")
elseif (debug STREQUAL "chkb")
  set(fortran_flags "${opt_flag_debug} -check bounds -fpe0 -ftrapuv ${linalg_flags} ${precision_flag} ${Fortran} ${mpi_flag} ${cppmach} ${cpprel} ${ADF}")
  set(c_flags       "${optc_flag_debug} ${linalg_cflags} ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${mpi_flag} ${cppmach} ${cpprel} -O2")
  set(cpp_flags     "${optc_flag_debug} ${linalg_cflags} ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${mpi_flag} ${cppmach} ${cpprel} -O2 -std=c++11")
elseif (debug STREQUAL "asan")
  set(fortran_flags "${opt_flag_debug} -fsanitize=address -DSANITIZE ${linalg_flags} ${precision_flag} ${Fortran} ${mpi_flag} ${cppmach} ${cpprel} ${ADF}")
  set(c_flags       "${optc_flag_debug} -fsanitize=address -DSANITIZE ${linalg_cflags} ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${mpi_flag} ${cppmach} ${cpprel} -O2")
  set(cpp_flags     "${optc_flag_debug} -fsanitize=address -DSANITIZE ${linalg_cflags} ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${mpi_flag} ${cppmach} ${cpprel} -O2 -std=c++11")
  set(asan_link     "-fsanitize=address")
else()
  set(fortran_flags "${opt_flag_release} ${linalg_flags} ${precision_flag} ${Fortran} ${mpi_flag} ${cppmach} ${cpprel} ${ADF}")
  set(c_flags       "${optc_flag_release} ${linalg_cflags} ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${mpi_flag} ${cppmach} ${cpprel} -O2")
  set(cpp_flags     "${optc_flag_release} ${linalg_cflags} ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${mpi_flag} ${cppmach} ${cpprel} -O2 -std=c++11")
endif()

set_source_files_properties(${source_files}     PROPERTIES COMPILE_FLAGS "${fortran_flags}")
set_source_files_properties(${c_source_files}   PROPERTIES COMPILE_FLAGS "${c_flags}")
set_source_files_properties(${cpp_source_files} PROPERTIES COMPILE_FLAGS "${cpp_flags}")

set(CMAKE_EXE_LINKER_FLAGS "-static-intel -rdynamic -qopenmp -qopenmp-link=static ${asan_link}")
string(STRIP "${CMAKE_EXE_LINKER_FLAGS}" CMAKE_EXE_LINKER_FLAGS)
